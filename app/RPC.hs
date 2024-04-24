{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module RPC where

import Binny qualified as BN
import Control.Monad
import Data.Bifunctor (second)
import Data.ByteString qualified as BS
import Data.IORef qualified as IOR
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Name (..), OccName (OccName))
import Network.Run.TCP qualified as RTCP
import Network.Socket.ByteString qualified as NSBS

argList :: Type -> [Type]
argList = \case
    ArrowT -> []
    e@ConT{} -> [e]
    AppT t1 t2 -> argList t1 <> argList t2
    x -> error $ show x

replaceArg :: Name -> Name -> Type -> Type
replaceArg n1 n = \case
    AppT t1 t2 -> AppT (replaceArg n1 n t1) (replaceArg n1 n t2)
    ConT n2 -> if n1 == n2 then ConT n else ConT n2
    ParensT t -> ParensT (replaceArg n1 n t)
    x -> x

ioUnitLast :: Type -> Type
ioUnitLast = \case
    AppT t1 t2 -> AppT t1 (ioUnitLast t2)
    ConT{} -> (AppT (ConT . mkName $ "IO") (ConT . mkName $ "()"))
    x -> x

ioLast :: Type -> Type
ioLast = \case
    AppT t1 t2 -> AppT t1 (ioLast t2)
    ConT t -> (AppT (ConT . mkName $ "IO") (ConT t))
    x -> x

getFuncCons :: Name -> [String] -> Q [(String, [BangType])]
getFuncCons struct strs = do
    info <- mapM (reify . mkName) strs
    forM info $ \case
        VarI (Name (OccName n) _) t _ -> do
            let list = argList t
            let mutate =
                    (\(ConT n) -> n) (head list) == struct
                        && (\(ConT n) -> n) (last list) == struct
            let args =
                    map
                        (Bang NoSourceUnpackedness NoSourceStrictness,)
                        ( if mutate
                            then init . tail $ list
                            else tail . tail $ list
                        )
            pure (n, args)
        _ -> error "did you call this on a non function?"

createDataStruct :: String -> [String] -> Q Dec
createDataStruct st strs = do
    (TyConI (NewtypeD _ (Name (OccName o1) f1) _ _ _ _)) <- reify . mkName $ st
    let struct = Name (OccName o1) f1
    let remoteStruct = mkName (o1 ++ "'R'Message")
    cons <- map (\(n, s) -> NormalC (mkName $ "R'" <> n) s) <$> getFuncCons struct strs
    let derive = DerivClause Nothing [ConT (mkName "Show"), ConT (mkName "Generic"), ConT (mkName "Binny")]
    return $ DataD [] remoteStruct [] Nothing cons [derive]

listToApp :: [Exp] -> Exp
listToApp [] = error "unreachable"
listToApp [a] = a
listToApp (a : bs) = AppE (listToApp bs) a

doesMutate :: Name -> Type -> Bool
doesMutate struct t =
    let list = argList t
     in (\(ConT n) -> n) (head list) == struct
            && (\(ConT n) -> n) (last list) == struct

createFunctions :: String -> [String] -> Q [Dec]
createFunctions st strs = do
    (TyConI (NewtypeD _ (Name (OccName o1) f1) _ _ _ _)) <- reify . mkName $ st
    let struct = Name (OccName o1) f1
    let remoteStruct = mkName (o1 ++ "'R")
    info <- mapM (reify . mkName) strs
    defs <- forM info $ \case
        VarI (Name (OccName n) _) t _ -> do
            let newType = replaceArg struct remoteStruct t
                name = mkName (n <> "'R")
                dataName = ConE $ mkName ("R'" <> n)
                list = argList t
                mutate = doesMutate struct t
                consLen = length list - 3
                vars = map (\c -> VarP . mkName $ "a" <> show c) [0 .. consLen]
                constructor = listToApp . reverse $ dataName : map (\c -> VarE . mkName $ "a" <> show c) [0 .. consLen]
                cons = pure constructor
                types = (if mutate then ioUnitLast else ioLast) newType
            call <-
                if mutate
                    then
                        [|
                            RTCP.runTCPClient addr port $ \s -> do
                                let item = BN.bin $cons
                                let len = BN.bin $ BS.length item
                                NSBS.sendAll s len
                                NSBS.sendAll s item
                                _ <- NSBS.recv s 1
                                pure ()
                            |]
                    else
                        [|
                            RTCP.runTCPClient addr port $ \s -> do
                                let item = BN.bin $cons
                                let len = BN.bin $ BS.length item
                                NSBS.sendAll s len
                                NSBS.sendAll s item
                                ansLen <- BN.debin <$> NSBS.recv s 4
                                ans <- NSBS.recv s ansLen
                                pure $ BN.debin ans
                            |]
            let lamArgs = [ConP remoteStruct [] [VarP . mkName $ "addr", VarP . mkName $ "port"]] <> vars
            pure
                [ SigD name types
                , FunD name [Clause [] (NormalB $ LamE lamArgs call) []]
                ]
        _ -> error "did you call this on a non function?"
    pure $ concat defs

createRemoteStruct :: String -> Q Dec
createRemoteStruct st = do
    (TyConI (NewtypeD _ (Name (OccName o1) _) _ _ _ _)) <- reify . mkName $ st
    let remoteStruct = mkName (o1 <> "'R")
    let cons =
            [ NormalC
                remoteStruct
                [ (Bang NoSourceUnpackedness NoSourceStrictness, ConT (mkName "String"))
                , (Bang NoSourceUnpackedness NoSourceStrictness, ConT (mkName "String"))
                ]
            ]
    return $ DataD [] remoteStruct [] Nothing cons []

generateArgs :: String -> [String] -> Exp
generateArgs start h = foldl AppE (VarE . mkName $ start) (map (VarE . mkName) h)

createServeFunc :: String -> [String] -> Q [Dec]
createServeFunc st strs = do
    (TyConI (NewtypeD _ (Name (OccName o1) f1) _ _ _ _)) <- reify . mkName $ st
    let struct = Name (OccName o1) f1
        name = mkName ("serve'" <> o1)
        lamArgs = [VarP . mkName $ "rawE"]
        messageStruct = pure . ConT $ mkName (o1 ++ "'R'Message")
    info <- mapM (reify . mkName) strs
    defs <- forM info $ \case
        VarI (Name _ _) t _ -> pure t
        _ -> error "non function"
    cons <-
        zip defs
            <$> ( map
                    ( second
                        (zipWith (\a _ -> "a" <> show a) [0 :: Int ..])
                    )
                    <$> getFuncCons struct strs
                )
    let mkCase eRef (t, (con, vars)) = do
            let mutate = doesMutate struct t
            save <- [e|IOR.writeIORef $(pure $ VarE eRef)|]
            let args = generateArgs con ("e'" : vars)
                call = pure (AppE save args)
                readRef = [e|(IOR.readIORef $(pure $ VarE eRef))|]
                e = mkName "e'"

            do' <-
                if mutate
                    then
                        [e|
                            do
                                $(pure $ VarP e) <- $readRef
                                $call
                                NSBS.sendAll s (BS.singleton 0)
                            |]
                    else
                        [e|
                            do
                                $(pure $ VarP e) <- $readRef
                                let ans = BN.bin ($(pure args))
                                let len = BN.bin (BS.length ans)
                                NSBS.sendAll s len
                                NSBS.sendAll s ans
                            |]
            pure $
                Match
                    (ConP (mkName $ "R'" <> con) [] (map (VarP . mkName) vars))
                    (NormalB do')
                    []
    let cases = do
            let msg = mkName "msg'"
            let eRef = mkName "eRef"
            c <- mapM (mkCase eRef) cons
            pure $ LamE [VarP msg, VarP eRef] (CaseE (VarE msg) c)
    call <-
        [|
            do
                e <- IOR.newIORef rawE
                void . RTCP.runTCPServer (Just "localhost") "8000" $ \s -> do
                    len <- BN.debin <$> NSBS.recv s 4
                    msg <- (BN.debin <$> NSBS.recv s len) :: IO $messageStruct
                    $(cases) msg e
            |]
    -- debug call
    types <- SigD name <$> [t|$(pure $ ConT $ mkName st) -> IO ()|]
    return
        [ types
        , FunD name [Clause [] (NormalB $ LamE lamArgs call) []]
        ]

expose :: String -> [String] -> Q [Dec]
expose s str = do
    serveFunc <- createServeFunc s str
    remoteStruct <- createRemoteStruct s
    dataStruct <- createDataStruct s str
    functions <- createFunctions s str
    pure $ remoteStruct : dataStruct : functions <> serveFunc

debug :: forall a b. (Show a) => a -> b
debug = error . show

-- mkCurryDec ith = do
--     let name = mkName $ "rem_" ++ ith
--     exp <- [|i + 1|]
--     return $ FunD name [Clause [] (NormalB exp) []]
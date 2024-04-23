{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module RPC where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Name (..), OccName (OccName))

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

ioLast :: Type -> Type
ioLast = \case
    AppT t1 t2 -> AppT t1 (ioLast t2)
    ConT{} -> (AppT (ConT . mkName $ "IO") (ConT . mkName $ "()"))
    x -> x

createDataStruct :: String -> [String] -> Q Dec
createDataStruct st strs = do
    (TyConI (NewtypeD _ (Name (OccName o1) f1) _ _ _ _)) <- reify . mkName $ st
    let struct = Name (OccName o1) f1
    let remoteStruct = mkName (o1 ++ "'R'Message")
    info <- mapM (reify . mkName) strs
    cons <- forM info $ \case
        VarI (Name (OccName n) _) t _ -> do
            let list = argList t
            let mutate =
                    (\(ConT n) -> n) (head list) == struct
                        && (\(ConT n) -> n) (last list) == struct
            unless mutate (error "can only use this on mutation operations")
            let args =
                    map
                        (Bang NoSourceUnpackedness NoSourceStrictness,)
                        (init . tail $ list)
            pure $ NormalC (mkName $ "R'" <> n) args
        _ -> error "did you call this on a non function?"
    let derive = DerivClause Nothing [ConT (mkName "Generic"), ConT (mkName "Binny")]
    return $ DataD [] remoteStruct [] Nothing cons [derive]

createFunctions :: String -> [String] -> Q [Dec]
createFunctions st strs = do
    (TyConI (NewtypeD _ (Name (OccName o1) f1) _ _ _ _)) <- reify . mkName $ st
    let struct = Name (OccName o1) f1
    let remoteStruct = mkName (o1 ++ "'R")
    info <- mapM (reify . mkName) strs
    defs <- forM info $ \case
        VarI (Name (OccName n) _) t _ -> do
            let newType = replaceArg struct remoteStruct t
            let name = mkName (n <> "'R")
            let list = argList t
            let mutate =
                    (\(ConT n) -> n) (head list) == struct
                        && (\(ConT n) -> n) (last list) == struct
            if mutate
                then do
                    let types = ioLast newType
                    -- error $ show newType ++ "\n" ++ show types
                    call <-
                        [|
                            runTCPClient addr port $ \s -> do
                                sendAll s (bin (R'updateCounter a1))
                                recv s 1
                                pure ()
                            |]
                    let lamArgs = [VarP . mkName $ "e", VarP . mkName $ "a1"]
                    pure
                        [ SigD name types
                        , FunD name [Clause [] (NormalB $ LamE lamArgs call) []]
                        ]
                else undefined
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

createRPC :: String -> [String] -> Q [Dec]
createRPC s str =
    ((:) <$> createRemoteStruct s)
        <*> (((:) <$> createDataStruct s str) <*> createFunctions s str)

-- mkCurryDec ith = do
--     let name = mkName $ "rem_" ++ ith
--     exp <- [|i + 1|]
--     return $ FunD name [Clause [] (NormalB exp) []]
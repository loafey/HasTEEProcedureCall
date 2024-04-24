e = DoE Nothing
    [ BindS (VarP e_6989586621682291167)
      (AppE (UnboundVarE newIORef) (UnboundVarE rawE))
    , NoBindS
      (InfixE (Just (InfixE (Just (VarE Data.Functor.void)) (VarE GHC.Base..)
                     (Just (AppE (AppE (UnboundVarE runTCPServer)
                                  (AppE (ConE GHC.Maybe.Just)
                                   (LitE (StringL "localhost"))))
                            (LitE (StringL "8000")))))) (VarE GHC.Base.$)
       (Just
        (LamE [ VarP s_6989586621682291168 ]
         (DoE Nothing
          [ BindS (VarP len_6989586621682291169)
            (InfixE (Just (UnboundVarE debin)) (VarE Data.Functor.<$>)
             (Just (AppE (AppE (UnboundVarE recv) (VarE s_6989586621682291168))
                    (LitE (IntegerL 4)))))
          , BindS (VarP msg_6989586621682291170)
            (SigE
             (InfixE (Just (UnboundVarE debin)) (VarE Data.Functor.<$>)
              (Just (AppE (AppE (UnboundVarE recv) (VarE s_6989586621682291168))
                     (VarE len_6989586621682291169))))
             (AppT (ConT GHC.Types.IO) (ConT Env'R'Message)))
          , NoBindS
            (AppE (LamE [ VarP msg ]
                   (CaseE (VarE msg)
                    [ Match (ConP updateCounter [] [ VarP a0 ])
                      (NormalB (VarE undefined)) []
                    , Match (ConP updateCounter2 [] [ VarP a0, VarP a1 ])
                      (NormalB (VarE undefined)) []
                    ])) (VarE msg_6989586621682291170))
          ]))))
    ]
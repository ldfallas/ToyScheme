module ScEval where

  import ScEnv
  import ScParser
  import ScPPrint

  import Control.Monad.Error

  

  -- | Evaluation
  evalExpr :: Expr -> ScInterpreterMonad Expr
  evalExpr number@(ScNumber _) = return number
  evalExpr expr = throwError "Not implemented"

  evalString :: String -> Env -> IO ()
  evalString code env =
   do
     r <- runErrorT $ parseAndEval code env
     case r of
        Left e -> print e
        Right expr -> print (renderExpr expr) 
     --print $ show r
     return ()
   where
      parseAndEval code env =
       do 
        parsed <- case parseIt code of
                    (Right exp) -> return exp
                    (Left x) -> throwError "Parse error"
        evalResult <- evalExpr parsed
        return evalResult
  --   let expr = parseIt code
         
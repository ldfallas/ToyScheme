module ScEval where

  import ScEnv
  import ScParser
  import ScPPrint

  import Control.Monad.Error

  isSpecialForm (ScSymbol name) = False
  isSpecialForm _ = False

  -- | Evaluation
  evalExpr :: Expr -> Env -> ScInterpreterMonad Expr
  evalExpr number@(ScNumber _) _ = return number
  evalExpr cons@(ScCons head rest) env | not $ isSpecialForm head =
       do 
         args <- consToList rest
         evaluatedArgs <- mapM (\e -> evalExpr e env) args
         toApply <- evalExpr head env
         apply toApply evaluatedArgs env  
  evalExpr (ScSymbol name) env =
     do
       bindingValue <- (liftIO $ lookupEnv env name)
       case bindingValue of
          Nothing -> throwError "Symbol not found"
          Just value -> return value
  evalExpr expr _ = throwError "Not implemented"

  apply :: Expr -> [Expr] -> Env -> ScInterpreterMonad Expr
  apply (ScPrimitive p) exprs env =
     p exprs 
  apply _ _ _ = throwError "Application error"

  consToList :: Expr -> ScInterpreterMonad [Expr]        
  consToList (ScCons next rest) = 
      do
        cdrList <- consToList rest
        return $ next : cdrList
  consToList ScNil = return []
  consToList _ = throwError "Argument list is not a proper list"

  evalStringOnRoot code =
     do
       rootEnv <- createRootEnv
       evalString code rootEnv
  

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
        evalResult <- evalExpr parsed env
        return evalResult
  --   let expr = parseIt code
         

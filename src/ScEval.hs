module ScEval where

  import ScEnv
  import ScParser
  import ScPPrint

  import Control.Monad.Error

  isSpecialForm :: String -> Bool
  isSpecialForm "define" = True
  isSpecialForm name = False


  -- | Evaluation
  evalExpr :: Expr -> Env -> ScInterpreterMonad Expr
  evalExpr number@(ScNumber _) _ = return number
  evalExpr symbol@(ScSymbol symbolName) env = 
      do
         symbolValue <- (liftIO $ lookupEnv env symbolName)
         case symbolValue of
            Nothing -> throwError $ "Symbol not found: " ++ symbolName
            Just value -> return value
         
  evalExpr (ScCons 
               (ScSymbol "define")
               (ScCons 
                   name@(ScSymbol symbolToDefine)
                   (ScCons
                      value
                      ScNil))) 
           env =
      evaluateDefineSymbol symbolToDefine value env

  evalExpr (ScCons 
               (ScSymbol "progn")
               rest) 
           env =
      do 
        expressions <- (consToList rest)
        evaluateExpressionSequence expressions  env


  evalExpr cons@(ScCons symbol@(ScSymbol name) rest) env | not $ isSpecialForm name =
       do 
         args <- consToList rest
         evaluatedArgs <- mapM (\e -> evalExpr e env) args
         toApply <- evalExpr symbol env
         apply toApply evaluatedArgs env  

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


  evaluateDefineSymbol :: String -> Expr ->  Env -> ScInterpreterMonad Expr
  evaluateDefineSymbol name value env =
     do
       evaluatedValue <- evalExpr value env
       (liftIO $ insertInCurrentEnv env name evaluatedValue)
       return ScNil
          
  evaluateExpressionSequence :: [Expr] ->  Env -> ScInterpreterMonad Expr
  evaluateExpressionSequence [] _ = return ScNil
  evaluateExpressionSequence [last] env = evalExpr last env
  evaluateExpressionSequence (expr:rest) env =
          do 
             _ <- evalExpr expr env
             evaluateExpressionSequence rest env
  

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
         

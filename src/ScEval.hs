module ScEval where

  import ScEnv
  import ScParser
  import ScPPrint

  import Control.Monad.Error

  isSpecialForm :: String -> Bool
  isSpecialForm "define" = True
  isSpecialForm "if" = True
  isSpecialForm "lambda" = True
  isSpecialForm name = False


  -- | Evaluation
  evalExpr :: Expr -> Env -> ScInterpreterMonad Expr
  evalExpr boolValue@(ScBool _) _ = return boolValue
  evalExpr number@(ScNumber _) _ = return number
  evalExpr str@(ScString _) _ = return str
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
               (ScSymbol "lambda")
               (ScCons 
                   (ScCons (ScSymbol argumentName) ScNil)
                   (ScCons
                      expr
                      ScNil))) 
           env =
    do
     return $  ScClosure [argumentName] expr env 


  evalExpr (ScCons 
               (ScSymbol "if")
               (ScCons condition
                  (ScCons
                     thenExpr
                     (ScCons
                        elseExpr
                        ScNil)))) 
           env =
      do 
        conditionEvaluation <- (evalExpr condition env)
        case conditionEvaluation of
          ScBool False -> evalExpr elseExpr env
          _ -> evalExpr thenExpr env
        



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

  evalExpr nil@ScNil _ = return nil

  evalExpr expr _ = throwError $ "Evaluation of \"" ++ (renderExpr expr) ++ "\" is not implemented"

  apply :: Expr -> [Expr] -> Env -> ScInterpreterMonad Expr
  apply (ScPrimitive p) exprs env =
     p exprs 
  apply (ScClosure arguments expr definitionEnv) exprs _ =
     do
        newEnv <- liftIO $ createNewEnvWithParent (zip arguments exprs) definitionEnv
        evalExpr expr newEnv
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
       evalStringAndPrint code rootEnv

  evalStringOnRootToString code =
     do
       rootEnv <- createRootEnv
       evalStringToString code rootEnv
  

  evalString :: String -> Env -> IO (Either String Expr)
  evalString code env =
   do
     runErrorT $ parseAndEval code env
   where
      parseAndEval code env =
       do 
        parsed <- case parseIt code of
                    (Right exp) -> return exp
                    (Left x) -> throwError "Parse error"
        evalResult <- evalExpr parsed env
        return evalResult

   
  evalStringToString :: String -> Env -> IO String
  evalStringToString codeString env =
     do 
       evalResult <- evalString codeString env
       case evalResult of
          Left e -> return $ "Error: " ++ e
          Right expr -> return $ renderExpr expr

  evalStringAndPrint :: String -> Env -> IO ()
  evalStringAndPrint codeString env =
     do 
        resultString <- evalStringToString codeString env
        print resultString
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


  data ScSeqExp =
     ScSeqLiteral (Expr ScSeqExp) 
     | ScSeqSymbol String
     | ScSeqApplication ScSeqExp [ScSeqExp] 
     | ScSeqLambdaCreation [String] ScSeqExp
     | ScSeqDefine String ScSeqExp
     | ScSeqExprSeq  [ScSeqExp]
     | ScConditional ScSeqExp ScSeqExp ScSeqExp  


  instance ScExecutable ScSeqExp where
     prepare  boolValue@(ScBool _) = ScSeqLiteral boolValue
     prepare number@(ScNumber _)  = ScSeqLiteral number
     prepare str@(ScString _)  = ScSeqLiteral str
     prepare (ScSymbol symbolName) = ScSeqSymbol symbolName
     prepare nil@ScNil = ScSeqLiteral nil
     prepare (ScCons 
               (ScSymbol "define")
               (ScCons 
                   name@(ScSymbol symbolToDefine)
                   (ScCons
                      value
                      ScNil))) = 
              ScSeqDefine symbolToDefine $ prepare value
     prepare (ScCons 
               (ScSymbol "lambda")
               (ScCons 
                   (ScCons (ScSymbol argumentName) ScNil)
                   (ScCons
                      expr
                      ScNil))) = 
             ScSeqLambdaCreation [argumentName] $ prepare expr 
     prepare  (ScCons 
               (ScSymbol "if")
               (ScCons condition
                  (ScCons
                     thenExpr
                     (ScCons
                        elseExpr
                        ScNil)))) = 
               ScConditional (prepare condition) (prepare thenExpr) (prepare elseExpr)
     prepare (ScCons 
               (ScSymbol "progn")
               rest) = ScSeqExprSeq $ map prepare (consToList rest)
     prepare cons@(ScCons symbol@(ScSymbol name) rest)  | not $ isSpecialForm name =
       ScSeqApplication (prepare symbol) $ map prepare (consToList rest)


     eval expr env =
        case expr of
         ScSeqLiteral literal -> return literal
         ScSeqSymbol symbolName ->
              do
                symbolValue <- (liftIO $ lookupEnv env symbolName)
                case symbolValue of
                   Nothing -> throwError $ "Symbol not found: " ++ symbolName
                   Just value -> return value

         ScSeqApplication callee args ->
             do 
                 evaluatedArgs <- mapM (\e -> eval e env) args
                 toApply <- eval callee env
                 apply toApply evaluatedArgs env  

         ScSeqLambdaCreation params body  ->
                return $ ScClosure params body env 
         ScSeqDefine symbolToDefine valueCode ->
                evaluateDefineSymbol symbolToDefine valueCode env
         ScSeqExprSeq expressionsCode ->
                evaluateCodeExpressionSequence expressionsCode env
         ScConditional condCode thenCode elseCode ->
               do 
                  conditionEvaluation <- (eval condCode env)
                  case conditionEvaluation of
                      ScBool False -> eval elseCode env
                      _ -> eval thenCode env

        -- _   -> throwError $ "Evaluation of \"" ++ "<expr not available>" ++ "\" is not implemented"


     apply (ScPrimitive p) exprs env = p exprs 
     apply (ScClosure arguments expr definitionEnv) exprs _ =
          do
             newEnv <- liftIO $ createNewEnvWithParent (zip arguments exprs) definitionEnv
             eval expr newEnv
     apply _ _ _ = throwError "Application error"



  -- | Evaluation
  evalExpr :: ScExecutable a => (Expr a) -> (Env a) -> ScInterpreterMonad (Expr a)
  evalExpr expr env =
    do 
      prepared <- return $ prepare expr
      eval prepared env
  consToList :: ScExecutable a => (Expr a) ->  [Expr a]        
  consToList (ScCons next rest) = next : consToList rest
  consToList ScNil =  []
  consToList other = [other]

  -- consToList :: ScExecutable a => (Expr a) -> ScInterpreterMonad [Expr a]        
  -- consToList (ScCons next rest) = 
  --     do
  --       cdrList <- consToList rest
  --       return $ next : cdrList
  -- consToList ScNil = return []
  -- consToList _ = throwError "Argument list is not a proper list"


  evaluateDefineSymbol :: ScExecutable a => String -> a ->  (Env a) -> ScInterpreterMonad (Expr a)
  evaluateDefineSymbol name value env =
     do
       evaluatedValue <- eval value env
       (liftIO $ insertInCurrentEnv env name evaluatedValue)
       return ScNil

  evaluateCodeExpressionSequence :: ScExecutable a => [a] ->  (Env a) -> ScInterpreterMonad (Expr a)
  evaluateCodeExpressionSequence [] _ = return ScNil
  evaluateCodeExpressionSequence [last] env = eval last env
  evaluateCodeExpressionSequence (expr:rest) env =
          do 
             _ <- eval expr env
             evaluateCodeExpressionSequence rest env

          
  evaluateExpressionSequence :: ScExecutable a =>[a] ->  (Env a) -> ScInterpreterMonad (Expr a)
  evaluateExpressionSequence [] _ = return ScNil
  evaluateExpressionSequence [last] env = eval last env
  evaluateExpressionSequence (expr:rest) env =
          do 
             _ <- eval expr env
             evaluateExpressionSequence rest env
  
  

  evalStringOnRoot code =
     do
       rootEnv  <- liftIO createRootEnv 
       ev code rootEnv
     where 
       ev ::   String -> (Env ScSeqExp ) -> IO ()
       ev = evalStringAndPrint

  evalStringOnRootToString code =
     do
       rootEnv <- createRootEnv
       ev code rootEnv
     where 
       ev ::   String -> (Env ScSeqExp ) -> IO String
       ev = evalStringToString
  

  evalString :: ScExecutable a => String -> (Env a) -> IO (Either String (Expr a))
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

   
  evalStringToString ::  ScExecutable a => String -> (Env a) -> IO String
  evalStringToString codeString env =
     do 
       evalResult <- evalString codeString env
       case evalResult of
          Left e -> return $ "Error: " ++ e
          Right expr -> return $ renderExpr expr

  evalStringAndPrint ::  ScExecutable a => String -> (Env a) -> IO ()
  evalStringAndPrint codeString env =
     do 
        resultString <- evalStringToString codeString env
        print resultString
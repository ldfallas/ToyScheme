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
     prepare  boolValue@(ScBool _) = 
                  return $ ScSeqLiteral boolValue

     prepare number@(ScNumber _)  = 
                  return $ ScSeqLiteral number

     prepare str@(ScString _)  = 
                  return $ ScSeqLiteral str

     prepare (ScSymbol symbolName) = 
                  return $ ScSeqSymbol symbolName

     prepare nil@ScNil = return $ ScSeqLiteral nil

     prepare (ScCons 
               (ScSymbol "define")
               (ScCons 
                   name@(ScSymbol symbolToDefine)
                   (ScCons
                      value
                      ScNil))) = 
              do
                 preparedValue <- prepare value
                 return $ ScSeqDefine symbolToDefine preparedValue 

     prepare (ScCons 
               (ScSymbol "lambda")
               (ScCons 
                   (ScCons (ScSymbol argumentName) ScNil)
                   (ScCons
                      expr
                      ScNil))) = 
             do
               preparedLambdaBody <- prepare expr 
               return $ ScSeqLambdaCreation [argumentName]  preparedLambdaBody

     prepare  (ScCons 
               (ScSymbol "if")
               (ScCons condition
                  (ScCons
                     thenExpr
                     (ScCons
                        elseExpr
                        ScNil)))) = 
            do
               prepCondition <- prepare condition
               prepThen <- prepare thenExpr
               prepElse <- prepare elseExpr
               return $ ScConditional prepCondition prepThen prepElse

     prepare (ScCons 
               (ScSymbol "progn")
               rest) = 
           do
              preparedExprs <- mapM prepare (consToList rest)
              return $ ScSeqExprSeq preparedExprs
     prepare cons@(ScCons symbol@(ScSymbol name) rest)  | not $ isSpecialForm name =
       do
         prepSymbol <- prepare symbol
         prepArgs   <- mapM prepare (consToList rest)
         return $ ScSeqApplication prepSymbol prepArgs


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
      prepared <- prepare expr
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
  
  


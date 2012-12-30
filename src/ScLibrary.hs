module ScLibrary where

  import ScEnv
  import ScEval
  import ScParser
  import ScPPrint
  import Data.IORef
  import Control.Monad.Error
  import Data.Foldable(foldr')
  import Control.Monad
  import System.IO(hPutStr,
                   openFile,
                   IOMode(WriteMode),
                   hClose)

  type DoubleFunc = Double -> Double -> Double
  type IntegerFunc = Integer -> Integer -> Integer
  

  applyNumericValue ::  ScExecutable a =>  Expr a -> Expr a -> DoubleFunc -> IntegerFunc -> ScInterpreterMonad (Expr a)
  applyNumericValue (ScNumber x) (ScNumber y) _ f =  return (ScNumber $ x `f` y)
  applyNumericValue (ScDouble x) (ScDouble y) f _ =  return (ScDouble $ x `f` y)
  applyNumericValue (ScDouble x) (ScNumber y) f _ =  return (ScDouble $ x `f` (fromInteger y))
  applyNumericValue (ScNumber x) (ScDouble y) f _ =  return (ScDouble $ (fromInteger x) `f` y)
  applyNumericValue _ _ _ _ = throwError "Incorrect plus arguments"

 
  addNumericValue ::  ScExecutable a => Expr a -> Expr a -> ScInterpreterMonad (Expr a)
  addNumericValue x y = applyNumericValue x y (+) (+)

  multiNumericValue :: ScExecutable a => Expr a -> Expr a -> ScInterpreterMonad (Expr a)
  multiNumericValue x y = applyNumericValue x y (*) (*)

  minusNumericValue ::  ScExecutable a => Expr a -> Expr a -> ScInterpreterMonad (Expr a)
  minusNumericValue x y = applyNumericValue x y (-) (-)

  --divNumericValue :: Expr -> Expr -> ScInterpreterMonad Expr
  --divNumericValue x y = applyNumericValue x y (/) (/)

  gtNumericValue ::  ScExecutable a => Expr a -> Expr a -> ScInterpreterMonad (Expr a)
  gtNumericValue (ScNumber x) (ScNumber y) =  return (ScBool $ x > y)
  gtNumericValue (ScDouble x) (ScDouble y) =  return (ScBool $ x > y)
  gtNumericValue (ScDouble x) (ScNumber y) =  return (ScBool $ x > (fromInteger y))
  gtNumericValue (ScNumber x) (ScDouble y) =  return (ScBool $ (fromInteger x) > y)
  gtNumericValue _ _ = throwError "Incorrect plus arguments"

  equalNumericValue ::  ScExecutable a => Expr a -> Expr a -> ScInterpreterMonad (Expr a)
  equalNumericValue (ScNumber x) (ScNumber y) =  return (ScBool $ x == y)
  equalNumericValue (ScDouble x) (ScDouble y) =  return (ScBool $ x == y)
  equalNumericValue (ScDouble x) (ScNumber y) =  return (ScBool $ x == (fromInteger y))
  equalNumericValue (ScNumber x) (ScDouble y) =  return (ScBool $ (fromInteger x) == y)
  equalNumericValue _ _ = throwError "Incorrect plus arguments"


  plusPrimitive ::  ScExecutable a => Expr a
  plusPrimitive =
    ScPrimitive plusCode
   where
     plusCode (first:rest) = foldM addNumericValue first rest 
     plusCode _ = return (ScNumber 0)             

  minusPrimitive :: ScExecutable a => Expr a
  minusPrimitive =
    ScPrimitive minusCode
   where
     minusCode (first:rest) = foldM minusNumericValue first rest 
     minusCode _ = return (ScNumber 0)             


  gtPrimitive :: ScExecutable a => Expr a
  gtPrimitive =
    ScPrimitive gtCode
   where
     gtCode [first,rest] = gtNumericValue first rest
     gtCode _ = return (ScBool False)             


  equalPrimitive :: ScExecutable a => Expr a
  equalPrimitive =
    ScPrimitive eqCode
   where
     eqCode [first,rest] = equalNumericValue first rest
     eqCode _ = return (ScBool False)             


  timesPrimitive :: ScExecutable a => Expr a
  timesPrimitive =
    ScPrimitive timesCode
   where
     timesCode (first:rest) = foldM multiNumericValue first rest 
     timesCode _ = return (ScNumber 1)             

  listPrimitiveList :: ScExecutable a => [(String,Expr a)]
  listPrimitiveList =
     [
        ("null?", ScPrimitive nullPrimitive),
        ("list", ScPrimitive listPrimitive),
        ("car", ScPrimitive carPrimitive),
        ("cdr", ScPrimitive cdrPrimitive)
     ]
   where
      nullPrimitive [first] = 
          case first of
            ScNil -> return $ ScBool True
            _     -> return $ ScBool False
      nullPrimitive _ = throwError "Incorrect number of arguments" 
      listPrimitive args = return $  foldr' (\current rest -> ScCons current rest) ScNil args
      carPrimitive [ScCons first _] = return first
      carPrimitive _ = throwError "Incorrect arguments for car"
      cdrPrimitive [ScCons _ rest] = return rest
      cdrPrimitive _ = throwError "Incorrect arguments for cdr"

--
  portPrimitiveList :: ScExecutable a => [(String,Expr a)]
  portPrimitiveList =
     [
        ("write", ScPrimitive writePrimitive),
        ("newline",ScPrimitive newlinePrimitive),
        ("open-output-file", ScPrimitive openOutputFilePrimitive),
        ("close-output-port", ScPrimitive closeHandlePrimitive)
     ]
   where
      writePrimitive [ first ] = 
          do
             liftIO $ putStr (renderExpr first)
             return ScNil 
      writePrimitive [ expr, (ScPort handle)] =
          do
             liftIO $ hPutStr handle $ renderExpr expr
             return ScNil        
      writePrimitive _ = throwError "Incorrect arguments for write"

      newlinePrimitive [  ] = 
          do
             liftIO $ putStr "\n"
             return ScNil 
      newlinePrimitive [ (ScPort handle)] =
          do
             liftIO $ hPutStr handle $ "\n"
             return ScNil        
      newlinePrimitive _ = throwError "Incorrect arguments for newline"

      openOutputFilePrimitive [ ScString fileName ] =
        -- TODO add error handling
        do
          handle <- liftIO $ openFile fileName WriteMode 
          return $ ScPort handle 
      openOutputFilePrimitive _ = throwError "Incorrect arguments for open-output-file"

      closeHandlePrimitive [ScPort handle] =
        do
          liftIO $ hClose handle
          return ScNil
      closeHandlePrimitive _ = throwError "Incorrect arguments for close primitive"
          

--       

  createRootEnv :: ScExecutable a => IO (Env a)
  createRootEnv  =
    do
      primitives <- return  ([("+", plusPrimitive),
                             ("-", minusPrimitive),
                             (">", gtPrimitive),
                             ("*", timesPrimitive),
                             ("=", equalPrimitive)]
                            ++ listPrimitiveList
                            ++ portPrimitiveList)
      bindingVars <- mapM (\(name,primitive) ->
                             do
                               primitiveRef <- newIORef primitive
                               return (name, primitiveRef))
                          primitives 
      bindings <- newIORef bindingVars
      parentEnv <- newIORef NullEnv
      return $ Env (bindings,parentEnv)



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



  
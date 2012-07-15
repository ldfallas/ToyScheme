

module ScEnv where

       import Data.IORef

       import Data.Foldable(foldr')
       import Control.Monad
       import Control.Monad.Error

       type ScInterpreterMonad = ErrorT String IO
       
       data Env = Env (IORef [(String,IORef Expr)], IORef Env)                  
                  | NullEnv

       data Expr = ScSymbol String
                   | ScString String
                   | ScNumber Integer
                   | ScDouble Double 
                   | ScCons Expr Expr
                   | ScNil
                   | ScBool Bool
                   | ScQuote Expr
                   | ScEnv
                   | ScClosure [String] Expr Env
                   | ScPrimitive ([Expr] -> ScInterpreterMonad Expr) 
        --    deriving Show

       type DoubleFunc = Double -> Double -> Double
       type IntegerFunc = Integer -> Integer -> Integer

       applyNumericValue :: Expr -> Expr -> DoubleFunc -> IntegerFunc -> ScInterpreterMonad Expr
       applyNumericValue (ScNumber x) (ScNumber y) _ f =  return (ScNumber $ x `f` y)
       applyNumericValue (ScDouble x) (ScDouble y) f _ =  return (ScDouble $ x `f` y)
       applyNumericValue (ScDouble x) (ScNumber y) f _ =  return (ScDouble $ x `f` (fromInteger y))
       applyNumericValue (ScNumber x) (ScDouble y) f _ =  return (ScDouble $ (fromInteger x) `f` y)
       applyNumericValue _ _ _ _ = throwError "Incorrect plus arguments"

      
       addNumericValue :: Expr -> Expr -> ScInterpreterMonad Expr
       addNumericValue x y = applyNumericValue x y (+) (+)

       multiNumericValue :: Expr -> Expr -> ScInterpreterMonad Expr
       multiNumericValue x y = applyNumericValue x y (*) (*)

       minusNumericValue :: Expr -> Expr -> ScInterpreterMonad Expr
       minusNumericValue x y = applyNumericValue x y (-) (-)

       --divNumericValue :: Expr -> Expr -> ScInterpreterMonad Expr
       --divNumericValue x y = applyNumericValue x y (/) (/)

       gtNumericValue :: Expr -> Expr -> ScInterpreterMonad Expr
       gtNumericValue (ScNumber x) (ScNumber y) =  return (ScBool $ x > y)
       gtNumericValue (ScDouble x) (ScDouble y) =  return (ScBool $ x > y)
       gtNumericValue (ScDouble x) (ScNumber y) =  return (ScBool $ x > (fromInteger y))
       gtNumericValue (ScNumber x) (ScDouble y) =  return (ScBool $ (fromInteger x) > y)
       gtNumericValue _ _ = throwError "Incorrect plus arguments"

       
       plusPrimitive =
         ScPrimitive plusCode
        where
          plusCode (first:rest) = foldM addNumericValue first rest 
          plusCode _ = return (ScNumber 0)             

       minusPrimitive =
         ScPrimitive minusCode
        where
          minusCode (first:rest) = foldM minusNumericValue first rest 
          minusCode _ = return (ScNumber 0)             


       gtPrimitive =
         ScPrimitive gtCode
        where
          gtCode [first,rest] = gtNumericValue first rest
          gtCode _ = return (ScBool False)             


       timesPrimitive =
         ScPrimitive timesCode
        where
          timesCode (first:rest) = foldM multiNumericValue first rest 
          timesCode _ = return (ScNumber 1)             

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
       

       createRootEnv  =
         do
           primitives <- return  ([("+", plusPrimitive),
                                  ("-", minusPrimitive),
                                  (">", gtPrimitive),
                                  ("*", timesPrimitive)]
                                 ++ listPrimitiveList)
           bindingVars <- mapM (\(name,primitive) ->
                                  do
                                    primitiveRef <- newIORef primitive
                                    return (name, primitiveRef))
                               primitives 
           bindings <- newIORef bindingVars
           parentEnv <- newIORef NullEnv
           return $ Env (bindings,parentEnv)

       createNewEnvWithParent :: [(String,Expr)] -> Env -> IO Env
       createNewEnvWithParent bindings parentEnv  =
         do
           plusRef <- newIORef plusPrimitive
           newVarBindings <- mapM (\(varName,expr) -> 
                                          do
                                            newBindingRef <- newIORef expr
                                            return (varName, newBindingRef )) 
                                   bindings
           bindingsRef <- newIORef newVarBindings 
           parentEnvRef <- newIORef parentEnv
           return $ Env (bindingsRef,parentEnvRef)


       lookupEnvValueRef :: Env -> String -> IO (Maybe (IORef Expr))
       lookupEnvValueRef env varName =
          case env of
           NullEnv -> return Nothing
           Env (bindingsRef,parentRef) ->
              do
                bindings <- readIORef bindingsRef
                parent <- readIORef parentRef
                case (lookup varName bindings)of
                 Just valRef -> return $ Just valRef
                 Nothing -> lookupEnvValueRef parent varName

       lookupEnv :: Env -> String -> IO (Maybe Expr)
       lookupEnv env varName =
              join (fmap  (maybe (return Nothing) ((>>= (return . Just)) . readIORef )) $ lookupEnvValueRef env varName) 

       lookupEnv3 env varName =
            fmap  ( liftM readIORef ) $ lookupEnvValueRef env varName

  --     lookupEnv8 env varName =
  --        (lookupEnvValueRef env varName)
  --         >>= (r -> case r of
  --                     Just rf -> (readIORef rf) >>= (rtf -> Just rtf))
       lookupEnv2 env varName = 
          do 
             r <- lookupEnvValueRef env varName
             case r of
               Just rf ->
                  do
                    rfv <- readIORef rf
                    return $ Just rfv
               Nothing -> return Nothing

       setValueInEnv env varName value =
           do 
             valueRefP <- lookupEnvValueRef env varName
             case valueRefP of
                Just valueRef -> 
                   do
                     writeIORef valueRef value
                     return env
                Nothing -> insertInCurrentEnv env varName value

       
       insertInCurrentEnv :: Env -> String -> Expr -> IO Env
       insertInCurrentEnv env varName value = 
        case env of
            NullEnv -> return env  -- TODO throw error
            Env (bindingsRef,_) ->
               do
                 bindings <- readIORef bindingsRef
                 newValueRef <- newIORef value
                 _ <- writeIORef bindingsRef ((varName, newValueRef):bindings)
                 return env
                  
--       insertInCurrentEnvM :: Env -> String -> Expr -> ScInterpreterMonad Env
--       insertInCurrentEnvM env varName value =
--         liftM (insertInCurrentEnv env varName value)
      

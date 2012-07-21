

module ScEnv where

       import Data.IORef

       import Data.Foldable(foldr')
       import Control.Monad
       import Control.Monad.Error

       type ScInterpreterMonad = ErrorT String IO


       class ScExecutable a where
          prepare :: (Expr a) -> a
          eval :: a -> (Env a) -> ScInterpreterMonad (Expr a)
          apply :: (Expr a) -> [(Expr a)] -> (Env a) -> ScInterpreterMonad (Expr a)

       
       data  ScExecutable a => Env a = Env (IORef [(String,IORef (Expr a))], IORef (Env a))                  
                  | NullEnv

       data ScExecutable a => Expr a = ScSymbol String
                   | ScString String
                   | ScNumber Integer
                   | ScDouble Double 
                   | ScCons (Expr a) (Expr a)
                   | ScNil
                   | ScBool Bool
                   | ScQuote (Expr a)
                   | ScEnv
                   | ScClosure [String] a (Env a)
                   | ScPrimitive ([Expr a] -> ScInterpreterMonad (Expr a)) 
        --    deriving Show

       
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
       

       createRootEnv :: ScExecutable a => IO (Env a)
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

       createNewEnvWithParent ::  ScExecutable a => [(String,Expr a)] -> Env a -> IO (Env a)
       createNewEnvWithParent bindings parentEnv  =
         do
           newVarBindings <- mapM (\(varName,expr) -> 
                                          do
                                            newBindingRef <- newIORef expr
                                            return (varName, newBindingRef )) 
                                   bindings
           bindingsRef <- newIORef newVarBindings 
           parentEnvRef <- newIORef parentEnv
           return $ Env (bindingsRef, parentEnvRef)


       lookupEnvValueRef ::  ScExecutable a => Env a -> String -> IO (Maybe (IORef (Expr a)))
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

       lookupEnv ::  ScExecutable a => (Env a) -> String -> IO (Maybe (Expr a))
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

       
       insertInCurrentEnv :: ScExecutable a =>(Env a) -> String -> (Expr a) -> IO (Env a)
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
      

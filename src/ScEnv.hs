
module ScEnv where

       import Data.IORef

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
                   | ScPrimitive ([Expr] -> ScInterpreterMonad Expr) 
        --    deriving Show
      
       addNumericValue :: Expr -> Expr -> ScInterpreterMonad Expr
       addNumericValue (ScNumber x) (ScNumber y) =  return (ScNumber $ x + y)
       addNumericValue (ScDouble x) (ScDouble y) =  return (ScDouble $ x + y)
       addNumericValue (ScDouble x) (ScNumber y) =  return (ScDouble $ x + (fromInteger y))
       addNumericValue (ScNumber x) (ScDouble y) =  return (ScDouble $ (fromInteger x) + y)
       addNumericValue _ _ = throwError "Incorrect plus arguments"
       
       plusPrimitive =
         ScPrimitive plusCode
        where
          plusCode (first:rest) = foldM addNumericValue first rest 
          plusCode _ = return (ScNumber 0)             

       createRootEnv  =
         do
           plusRef <- newIORef plusPrimitive
           bindings <- newIORef [("+",plusRef)]
           parentEnv <- newIORef NullEnv
           return $ Env (bindings,parentEnv)

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

       insertInCurrentEnv env varName value =
        case env of
            NullEnv -> return env  -- TODO throw error
            Env (bindingsRef,_) ->
               do
                 bindings <- readIORef bindingsRef
                 newValueRef <- newIORef value
                 _ <- writeIORef bindingsRef ((varName, newValueRef):bindings)
                 return env
                  
      


{-# LANGUAGE GADTs #-} 

module ScEnv where

       import Data.IORef

       import Data.Foldable(foldr')
       import Control.Monad
       import Control.Monad.Error

       type ScInterpreterMonad = ErrorT String IO


       class ScExecutable a where
          prepare :: (Expr a) -> ScInterpreterMonad a
          eval :: a -> (Env a) -> ScInterpreterMonad (Expr a)
          apply :: (Expr a) -> [(Expr a)] -> (Env a) -> ScInterpreterMonad (Expr a)

       
       data  Env a where
         Env :: (IORef [(String,IORef (Expr a))], IORef (Env a))                   -> Env a
         NullEnv :: Env a

       data Expr a where
               ScSymbol :: String -> Expr a
               ScString :: String -> Expr a
               ScNumber :: Integer -> Expr a
               ScDouble :: Double -> Expr a 
               ScCons :: (Expr a) -> (Expr a) -> (Expr a)
               ScNil :: Expr a
               ScBool :: Bool -> Expr a
               ScQuote :: (Expr a) -> Expr a
               ScEnv ::  (Expr a)
               ScClosure :: ScExecutable a =>  [String] -> a -> (Env a) -> Expr a
               ScPrimitive ::  ([Expr a] -> ScInterpreterMonad (Expr a))  -> Expr a
        --    deriving Show

       

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
      

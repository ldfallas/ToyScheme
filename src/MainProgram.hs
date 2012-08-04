module Main where
    
  import System.Environment ( getArgs )
  import Control.Monad.Error ( runErrorT )
  import ScLibrary
  import ScEval
  import ScParser
  import ScEnv

  main =
   do
    args <- getArgs
    case args of
       [fileName] ->
           do 
              contents <- readFile fileName
              fileParts <- return $ parseFileContents contents
              case fileParts of
                 Right asts ->
                    do
                       env <- createRootEnv
                       evalResult <- runErrorT $ evs [prepare x | x <- asts] env
                       return ()
                 _ ->
                    print "Parse error reading file"
              
       _ -> print "Incorrect number of argument for application"
   
   where
       evs :: [ScSeqExp] ->  (Env ScSeqExp) -> ScInterpreterMonad (Expr ScSeqExp)
       evs exprs env =
         evaluateExpressionSequence exprs env

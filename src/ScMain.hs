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
                       evalResult <- runErrorT $ evs asts env
                       case evalResult of
                         Left error -> print error
                         _ -> return ()
                 _ ->
                    print "Parse error reading file"
              
       _ -> print "Incorrect number of argument for application"
   
   where
       evs :: [Expr ScSeqExp] ->  (Env ScSeqExp) -> ScInterpreterMonad (Expr ScSeqExp)
       evs exprs env =
         do
             prepExprs <- mapM prepare exprs
             evaluateExpressionSequence prepExprs env

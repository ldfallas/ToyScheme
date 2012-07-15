import Test.HUnit
import ScParser
import ScEnv
import ScEval
import Text.Parsec



testNumLiteralEval =
    TestCase
       (do
         evalResult <- evalStringOnRootToString "10"  
         (assertEqual "Eval numeric literal"
                 "10"
                 evalResult))

testStringLiteralEval =
    TestCase
       (do
         evalResult <- evalStringOnRootToString "\"hola\""  
         (assertEqual "Eval string  literal"
                 "\"hola\""                 
                 evalResult))
               

testPrimitiveApplication =
    TestCase
       (do
         evalResult <- evalStringOnRootToString "(+ 15 5)"  
         (assertEqual "Eval basic primitive application"
                 evalResult
                 "20"))
               
               
testFactorial1 =
    TestCase
       (do
         program <- return $ "(progn " ++   
                             "  (define factorial " ++
                             "     (lambda (x) " ++
                             "       (if (> x 0) " ++
                             "           (* x (factorial (- x 1)))" ++
                             "           1)))" ++
                             "  (factorial 5))"
         evalResult <- evalStringOnRootToString program  
         (assertEqual "Eval basic factorial implementation"
                 "120"
                 evalResult
                 ))

               

tests = TestList [testNumLiteralEval,
                  testStringLiteralEval,
                  testPrimitiveApplication,
                  testFactorial1]

main =
     do runTestTT tests

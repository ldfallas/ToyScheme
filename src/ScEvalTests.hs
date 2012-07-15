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
                 evalResult
                 "10"))

testStringLiteralEval =
    TestCase
       (do
         evalResult <- evalStringOnRootToString "\"hola\""  
         (assertEqual "Eval string  literal"
                 evalResult
                 "\"hola\""))
               

testPrimitiveApplication =
    TestCase
       (do
         evalResult <- evalStringOnRootToString "(+ 15 5)"  
         (assertEqual "Eval basic primitive application"
                 evalResult
                 "20"))
               
               

               

tests = TestList [testNumLiteralEval,
                  testStringLiteralEval,
                  testPrimitiveApplication]

main =
     do runTestTT tests

import Test.HUnit
import ScParser
import ScEnv
import ScEval
import Text.Parsec

compareResult :: Either ParseError (Expr ScSeqExp) -> (Expr ScSeqExp) -> Bool
compareResult (Right x) y = compareAst x y
compareResult _ _ = False

compareAst ::  (Expr ScSeqExp) -> (Expr ScSeqExp) -> Bool
compareAst (ScSymbol x) (ScSymbol y) | x == y = True
compareAst (ScNumber x) (ScNumber y) | x == y = True
compareAst (ScBool x)   (ScBool y) | x == y = True
compareAst (ScDouble x) (ScDouble y) | x == y = True
compareAst (ScString x) (ScString y) | x == y = True
compareAst (ScCons x1 y1) (ScCons x2 y2) 
           | (compareAst x1 x2) 
             && (compareAst y1 y2)   = True
compareAst ScNil ScNil = True
compareAst _ _ = False  
  

testSymbol = TestCase 
               (assertBool "Parse simple symbol" 
                           (compareResult (parseIt "x")
                                          (ScSymbol "x")))
testSymbolPlus =
             TestCase 
               (assertBool "Parse simple symbol '+'" 
                           (compareResult (parseIt "+")
                                          (ScSymbol "+")))

testString =
             TestCase
               (assertBool "Parse simple string " 
                           (compareResult (parseIt "\"scheme\"")
                                          (ScString "scheme")))


testBool =
             TestCase
               (assertBool "Parse simple boolean " 
                           (compareResult (parseIt "#t")
                                          (ScBool True)))

testInt = TestCase 
               (assertBool "Parse simple number" 
                           (compareResult (parseIt "1")
                                          (ScNumber 1)))

testDouble = TestCase 
               (assertBool "Parse simple double number" 
                           (compareResult (parseIt "1.1")
                                          (ScDouble 1.1)))
testSinglePar = TestCase
                 (assertBool "Parse simple parentheses expression"
                      (compareResult
                         (parseIt "(+ 1 2)")
                         $ ScCons (ScSymbol "+")
                          $ ScCons (ScNumber 1)
                           $ ScCons (ScNumber 2)
                            $ ScNil))
                                                      

testNestedPar = TestCase
                 (assertBool "Parse nested parentheses expression"
                      (compareResult
                         (parseIt "(string-append \"a\" (string-append \"b\" \"A\"))")
                         $ ScCons (ScSymbol "string-append")
                          $ ScCons (ScString "a")
                           $ ScCons (ScCons
                                       (ScSymbol "string-append")
                                       (ScCons (ScString "b")
                                         $ ScCons
                                             (ScString "A") 
                                             ScNil))
                            $ ScNil))


testDottedList = TestCase
                 (assertBool "Parse simple dotted  parentheses expression"
                      (compareResult
                         (parseIt "(1 . 2)")
                          $ ScCons (ScNumber 1)
                            $ ScNumber 2))
                                                      



tests = TestList [testSymbol,
                  testSymbolPlus,
                  testBool,
                  testString,
                  testInt,
                  testDouble,
                  testSinglePar,
                  testDottedList,
                  testNestedPar]

main =
     do runTestTT tests

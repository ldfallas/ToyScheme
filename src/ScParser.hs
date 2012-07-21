module ScParser where


       import Text.Parsec
       import Text.Parsec.Token
       import Text.Parsec.Language
       import Data.Functor.Identity
       import ScEnv

--       data Expr  = ScSymbol String
--                   | ScString String
--                   | ScNumber Integer
--                   | ScDouble Double 
--                   | ScCons Expr  Expr 
--                   | ScNil
--                   | ScBool Bool
--                   | ScQuote Expr 
--                   | ScEnv
--

          

       idSymbol = oneOf ":!$%&*+/<=>?@\\^|-~"

       schemeLanguageDef :: LanguageDef st
       schemeLanguageDef = emptyDef { 
                          commentLine = ";;"
                          , identStart = letter <|> idSymbol
                          , identLetter = alphaNum <|> idSymbol
                          , opStart = parserZero
                          , opLetter = parserZero
                        }

       schemeTokenParser = makeTokenParser schemeLanguageDef

       TokenParser {
          identifier = idParser,
          reservedOp = opParser,
          stringLiteral = stringLiteralParser,
          parens = parParser,
          lexeme = lexParser,
          naturalOrFloat = naturalOrFloatParser
       } = schemeTokenParser

       type ScParserExprType a =  ParsecT String () Data.Functor.Identity.Identity (Expr a)

       boolLiteral :: ScExecutable a => ScParserExprType a
       boolLiteral = lexParser (
                        do 
                          char '#'
                          val <- (char 't') <|> (char 'f')
                          return $ ScBool $ val == 't'
                    )

       quoteParser :: ScExecutable a => ScParserExprType a
       quoteParser = lexParser (
                        do 
                          char '\''
                          val <- expressionParser
                          return $ ScQuote val
                    )


       dotParser = lexParser $ char '.'

       atom :: ScExecutable a => ScParserExprType a
       atom =
          (do 
             id <- idParser
             return $ ScSymbol id)
          <|>
           (do 
              fnumber <- naturalOrFloatParser
              return $ case fnumber of
                       Left num -> ScNumber num
                       Right num -> ScDouble num)
          <|>        
           (do str <- stringLiteralParser
               return $ ScString str)
          <|> boolLiteral
          <|> quoteParser

       dottedSuffixParser :: ScExecutable a => ScParserExprType a
       dottedSuffixParser =     
          do 
             dotParser
             finalExpr <- expressionParser
             return finalExpr

       parExpressionParser :: ScExecutable a => ScParserExprType a
       parExpressionParser = 
          do (exprs, last) <- parParser 
                       (do 
                          seq <- many expressionParser
                          dottedSuffix <- optionMaybe dottedSuffixParser 
                          return (case dottedSuffix of
                                   Just lastExpr -> (seq, lastExpr)
                                   Nothing -> (seq, ScNil)))
             return $ foldr ScCons last exprs

       expressionParser :: ScExecutable a => ScParserExprType a
       expressionParser =
            atom <|> parExpressionParser
            
       parseIt :: ScExecutable a => String -> Either ParseError (Expr a)
       parseIt input = parse expressionParser "" input

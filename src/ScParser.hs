module ScParser where


       import Text.Parsec
       import Text.Parsec.Token
       import Text.Parsec.Language
       import ScEnv

          

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

       boolLiteral = lexParser (
                        do 
                          char '#'
                          val <- (char 't') <|> (char 'f')
                          return $ ScBool $ val == 't'
                    )

       quoteParser = lexParser (
                        do 
                          char '\''
                          val <- expressionParser
                          return $ ScQuote val
                    )


       dotParser = lexParser $ char '.'

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

       dottedSuffixParser =     
          do 
             dotParser
             finalExpr <- expressionParser
             return finalExpr

       parExpressionParser = 
          do (exprs, last) <- parParser 
                       (do 
                          seq <- many expressionParser
                          dottedSuffix <- optionMaybe dottedSuffixParser 
                          return (case dottedSuffix of
                                   Just lastExpr -> (seq, lastExpr)
                                   Nothing -> (seq, ScNil)))
             return $ foldr ScCons last exprs

       expressionParser =
            atom <|> parExpressionParser
            

       parseIt input = parse expressionParser "" input

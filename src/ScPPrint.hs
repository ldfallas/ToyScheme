module ScPPrint where

    import ScEnv
    import ScParser
    import Text.PrettyPrint.HughesPJ 


    toStringP :: ScExecutable a => Expr a -> Doc
    toStringP (ScSymbol name) = text name
    toStringP (ScNumber num) = integer num
    toStringP (ScDouble num) = double num
    toStringP ScNil = text "()"
    toStringP (ScQuote exp) = text "'" <> (toStringP exp)
    toStringP (ScBool value) = text $ if value then "#t" else "#f"
    toStringP (ScString str) = doubleQuotes $ text str
    toStringP (ScCons first rest) = parens ( hang (toStringP first) 3  (sep $ toStringPCons rest))
      where
         toStringPCons (ScCons part rest) =  ((toStringP part) : (toStringPCons rest))
         toStringPCons ScNil = []
         toStringPCons other = [(text ".") , (toStringP other) ]


    renderExpr :: ScExecutable a => Expr a -> String
    renderExpr = (renderStyle style) . toStringP

    -- parseAndPrint text =
    --    case (parseIt text) of
    --      Right parsed -> Just $ toStringP parsed
    --      _ -> Nothing


    -- parseAndRender style text =
    --    case (parseIt text) of
    --      Right parsed -> putStr $ renderStyle style  $ toStringP parsed
    --      _ -> putStr "ParseError"


    -- smallTest style =
    --   parseAndRender style "(html (head (title \"Hello world\")) (body (p \"Hi\") (ol (li \"First\") (li \"Second\"))))"

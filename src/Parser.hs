{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Parser ( slim
              , module Parser.Types
              ) where
  import Parser.Types
  import Parser.Internal
  import Parser.Whitespace
  import Parser.HTML

  import Text.Megaparsec
  import Text.Megaparsec.String
  import qualified Text.Megaparsec.Lexer as L

  slim :: Parser Tree
  slim = tree <* eof

  tree :: Parser Tree
  tree = slimTree <$> (many . nonIndented) node

  node :: Parser SlimNode
  node = commentNode
        <|> embeddedEngineNode
        <|> codeNode
        <|> htmlNode
        <|> verbatimTextNode
        <?> "an HTML element, text, embedded code, comment"

  htmlNode :: Parser SlimNode
  htmlNode = indentBlock $ do
    topNode <- Node <$> elementName
    shorthands <- many (dotClass <|> hashId)
    whitespace <- slimWhitespace
    attrs <- ((++) shorthands) <$> attributes
    inlineContent <- inlineNodeContent
    return $ (pure . (slimNode whitespace) . (topNode attrs) .
      slimTree . (inlineContent ++)) `manyIndented` node

  inlineNodeContent :: Parser [SlimNode]
  inlineNodeContent =
        (pure <$> (embeddedCode >>= ($ mempty)))
    <|> (pure . (slimNode NoWhitespace) . VerbatimTextNode) <$> untilNewline
    <|> (pure mempty)
    where
      untilNewline = (noneOf "\n") `someTill` lookAhead newline

  codeNode :: Parser SlimNode
  codeNode = indentBlock $
    pure . (`manyIndented` node) =<< embeddedCode

  embeddedCode :: Parser ([SlimNode] -> Parser SlimNode)
  embeddedCode =
        (char '-'    *> (whitespacedCode (ControlCode <$> restOfLine)))
    <|> (string "==" *> (whitespacedCode (UnescapedCode <$> restOfLine)))
    <|> (char '='    *> (whitespacedCode (EscapedCode <$> restOfLine)))
    where
      whitespacedCode = (code <$> (slimWhitespace) <*>)
      code :: Whitespace -> EmbeddedCode -> [SlimNode] -> Parser SlimNode
      code w c = (pure . (slimNode w) . (EmbeddedCodeNode c) . slimTree)

  verbatimTextNode :: Parser SlimNode
  verbatimTextNode =
    (slimNode NoWhitespace) <$> VerbatimTextNode <$> textBlock "|"
    <|> (slimNode Trailing) <$> VerbatimTextNode <$> textBlock "'"

  commentNode :: Parser SlimNode
  commentNode = (slimNode NoWhitespace) <$> CommentNode
    <$> (HtmlComment <$> textBlock "\\!"
    <|>  SlimComment <$> textBlock "\\")

  embeddedEngineNode :: Parser SlimNode
  embeddedEngineNode = do
    topIndent <- L.indentLevel
    try $ (slimNode NoWhitespace) <$> (EmbeddedEngineNode
      <$> some letterChar
      <*> (char ':' *> newline *>
            (((anyChar `indentedBy` topIndent) `strippedOff`) =<< whitespace)))
    where
      whitespace = length <$> lookAhead (many spaceOrTab)

  attributes :: Parser [Attr]
  attributes = between (symbol "(") (symbol ")") multilineAttrs
           <|> between (symbol "[") (symbol "]") multilineAttrs
           <|> between (symbol "{") (symbol "}") multilineAttrs
           <|> inlineAttrs
    where
      multilineAttrs :: Parser [Attr] =
        (attributeName >>= \n -> (attribute n <|> (pure . BooleanAttr) n))
        `sepBy` many spaceChar
      inlineAttrs :: Parser [Attr] =
        try (attributeName >>= attribute) `sepBy` many spaceOrTab

  dotClass :: Parser Attr
  dotClass = (EscapedAttr "class") <$> (char '.' *> shorthandName)

  hashId :: Parser Attr
  hashId = (EscapedAttr "id") <$> (char '#' *> shorthandName)

  attribute :: String -> Parser Attr
  attribute name =
    string "==" *>
      (UnescapedAttr name <$> quotedString
      <|> UnescapedCodeAttr name <$> inlineCode)
    <|> char '=' *>
      (EscapedAttr name <$> quotedString
      <|> EscapedCodeAttr name <$> inlineCode)

  inlineCode :: Parser String
  inlineCode = lexeme $ concatSome (unterminatedLine
    <|> inside '(' ')' <|> inside '{' '}' <|> inside '[' ']')
    where
      unterminatedLine = some (noneOf "\" \t\n(){}[]")
      concatSome = (concat <$>) . some
      inside :: Char -> Char -> Parser String
      inside o c = do
        oc <- char o
        t <- concatSome (some (noneOf [o, c]) <|> inside o c)
        cc <- char c
        return (oc : (t ++ [cc]))

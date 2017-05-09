{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Parser where
  import Parser.Internal

  import Text.Megaparsec
  import Text.Megaparsec.String
  import qualified Text.Megaparsec.Lexer as L

  newtype Tree = Tree [Node] deriving (Eq, Show)

  data Node = Node String [Attr] Tree
            | EmbeddedCodeNode EmbeddedCode Tree
            | VerbatimTextNode String
            | CommentNode Comment
            | EmbeddedEngineNode String String
    deriving (Eq, Show)

  data Attr = EscapedCodeAttr String String
            | UnescapedCodeAttr String String
            | EscapedAttr String String
            | UnescapedAttr String String
            | BooleanAttr String
    deriving (Eq, Show)

  data EmbeddedCode = ControlCode String
                    | EscapedCode String
                    | UnescapedCode String
    deriving (Eq, Show)

  data Comment = SlimComment String
               | HtmlComment String
    deriving (Eq, Show)

  slim :: Parser Tree
  slim = tree <* eof

  tree :: Parser Tree
  tree = Tree <$> many (nonIndented node)

  node :: Parser Node
  node = commentNode
        <|> embeddedEngineNode
        <|> codeNode
        <|> htmlNode
        <|> verbatimTextNode
        <?> "an HTML element, text, embedded code, comment"

  htmlNode :: Parser Node
  htmlNode = indentBlock $ do
    topNode <- Node <$> elementName <*> attrs
    inlineContent <- inlineNodeContent
    return $ (pure . topNode . Tree . (inlineContent ++)) `manyIndented` node
      where
        attrs = (++) <$> shorthandAttributes <*> attributes
        shorthandAttributes = many (dotClass <|> hashId)

  inlineNodeContent :: Parser [Node]
  inlineNodeContent =
    (pure . (flip EmbeddedCodeNode) (Tree [])) <$> embeddedCode
    <|> (pure . VerbatimTextNode) <$> untilNewline
    <|> (pure mempty)
    where
      untilNewline = (noneOf "\n") `someTill` lookAhead newline

  codeNode :: Parser Node
  codeNode = indentBlock $ do
    code <- embeddedCode
    return $ (pure . EmbeddedCodeNode code . Tree) `manyIndented` node

  embeddedCode :: Parser EmbeddedCode
  embeddedCode = ControlCode   <$> (char '-' *> restOfLine)
             <|> UnescapedCode <$> (string "==" *> restOfLine)
             <|> EscapedCode   <$> (char '=' *> restOfLine)

  verbatimTextNode :: Parser Node
  verbatimTextNode = VerbatimTextNode <$> textBlock "|"

  commentNode :: Parser Node
  commentNode = CommentNode <$> (
        HtmlComment <$> textBlock "\\!"
    <|> SlimComment <$> textBlock "\\")

  embeddedEngineNode :: Parser Node
  embeddedEngineNode = do
    topIndent <- L.indentLevel
    try $ EmbeddedEngineNode
      <$> some letterChar
      <*> (char ':' *> newline *>
            (((anyChar `indentedBy` topIndent) `strippedOff`) =<< whitespace))
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

  -- Name     ::= (Letter | '_' | ':') (NameChar)*
  -- NameChar	::= Letter | Digit | '.' | '-' | '_' | ':'
  --
  -- Note that we can't use dots in element names;
  -- they're reserved for Slim's attribute shorthand notation.
  elementName :: Parser String
  elementName = lexeme $
    (:) <$> (letterChar <|> oneOf "_:")
        <*> many (alphaNumChar <|> oneOf "-_:")

  attributeName :: Parser String
  attributeName = lexeme $ some (alphaNumChar <|> oneOf "@-_:")

  shorthandName :: Parser String
  shorthandName = lexeme $ some (alphaNumChar <|> oneOf "_-")

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

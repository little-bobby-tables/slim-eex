{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Parser where
  import Control.Applicative (empty)
  import Control.Monad (void)
  import Text.Megaparsec
  import Text.Megaparsec.String
  import qualified Text.Megaparsec.Lexer as L

  import Data.List.Split (splitOn)

  newtype Tree = Tree [Node] deriving (Eq, Show)

  data Node = Node String [Attr] Tree
            | EmbeddedCodeNode EmbeddedCode Tree
            | VerbatimTextNode String
            | CommentNode Comment
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
  tree = Tree <$> many (L.nonIndented scn node)

  node :: Parser Node
  node = commentNode
        <|> codeNode
        <|> htmlNode
        <|> verbatimTextNode
        <?> "an HTML element, text, embedded code, comment"

  htmlNode :: Parser Node
  htmlNode = L.indentBlock scn $ do
    topNode <- Node <$> htmlEntityName <*> attrs
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
  codeNode = L.indentBlock scn $ do
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

  textBlock :: String -> Parser String
  textBlock separator = do
    separatorIndent <- string separator *> L.indentLevel
    textIndent <- leadingNewlines *> whitespaceLength
              <|> addIndent separatorIndent <$> whitespaceLength
    firstLine <- anyChar `manyTill` newline
    indentedLines <- splitOn "\n" <$> indentedBy separatorIndent
    return $ firstLine ++ (drop textIndent =<< indentedLines)
    where
      leadingNewlines = some newline
      whitespaceLength = length <$> many spaceOrTab
      addIndent = (+) . subtract 1 . fromIntegral . unPos
      indentedBy = (anyChar `manyTill`) . try . (L.indentGuard scn LT)

  restOfLine :: Parser String
  restOfLine = optional spaceOrTab *> anyChar `manyTill` lookAhead newline

  attributes :: Parser [Attr]
  attributes = between (symbol "(") (symbol ")") multilineAttrs
           <|> between (symbol "[") (symbol "]") multilineAttrs
           <|> between (symbol "{") (symbol "}") multilineAttrs
           <|> inlineAttrs
    where
      multilineAttrs :: Parser [Attr] =
        (htmlEntityName >>= \n -> (attribute n <|> (pure . BooleanAttr) n))
        `sepBy` many spaceChar
      inlineAttrs :: Parser [Attr] =
        try (htmlEntityName >>= attribute) `sepBy` many spaceOrTab

  dotClass :: Parser Attr
  dotClass = (EscapedAttr "class") <$> (char '.' *> htmlEntityName)

  hashId :: Parser Attr
  hashId = (EscapedAttr "id") <$> (char '#' *> htmlEntityName)

  htmlEntityName :: Parser String
  htmlEntityName = lexeme $ some (alphaNumChar <|> char '_' <|> char '-')

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

  quotedString :: Parser String
  quotedString = lexeme $ char '"' *> many quotedChar <* char '"'
    where
      quotedChar :: Parser Char =
        unescaped <|> escaped
      unescaped = noneOf "\\\""
      escaped = char '\\' *> oneOf "\\\""

  manyIndented :: ([b] -> Parser a) -> Parser b -> L.IndentOpt Parser a b
  manyIndented = L.IndentMany Nothing

  symbol :: String -> Parser String
  symbol = L.symbol sc

  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme sc

  sc :: Parser ()
  sc = L.space (void spaceOrTab) empty empty

  scn :: Parser ()
  scn = L.space (void spaceChar) empty empty

  spaceOrTab :: Parser Char
  spaceOrTab = oneOf " \t"

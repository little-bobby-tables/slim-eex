{-# LANGUAGE TypeFamilies, TupleSections, ScopedTypeVariables #-}

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

  newtype Attr = Attr (String, String) deriving (Eq, Show)

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
    nodeText <- optionalText
    return $ (pure . topNode . Tree . (nodeText ++)) `manyIndented` node
      where
        attrs = (++) <$> shorthandAttributes <*> attributes
        shorthandAttributes = many (dotClass <|> hashId)
        optionalText :: Parser [Node] =
          (pure . VerbatimTextNode) <$> untilNewline <|> pure mempty
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
      multilineAttrs :: Parser [Attr]
      multilineAttrs = attr
        (lexeme (char '=') *> quotedString <|> pure "")
        `sepBy` many spaceChar
      inlineAttrs :: Parser [Attr]
      inlineAttrs = try (attr
        (lexeme (char '=') *> quotedString))
        `sepBy` many spaceOrTab
      attr :: Parser String -> Parser Attr
      attr value = Attr <$> ((,) <$> htmlEntityName <*> value)

  dotClass :: Parser Attr
  dotClass = (Attr . ("class",)) <$> (char '.' *> htmlEntityName)

  hashId :: Parser Attr
  hashId = (Attr . ("id",)) <$> (char '#' *> htmlEntityName)

  htmlEntityName :: Parser String
  htmlEntityName = lexeme $ some (alphaNumChar <|> char '_' <|> char '-')

  quotedString :: Parser String
  quotedString = lexeme $ char '"' *> many quotedChar <* char '"'
    where
      quotedChar :: Parser Char
      quotedChar = unescaped <|> escaped
        where
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

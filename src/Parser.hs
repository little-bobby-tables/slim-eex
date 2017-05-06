{-# LANGUAGE TypeFamilies, TupleSections #-}

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
    name <- htmlEntityName
    shorthandAttrs <- many (dotClass <|> hashId)
    attrs <- (shorthandAttrs ++) <$> (attributes <|> pure [])
    text <- nodeText
    return $ L.IndentMany Nothing
      ((Node name attrs <$>) . (pure . Tree . (text ++))) node
      where
        nodeText :: Parser [Node]
        nodeText = (pure . VerbatimTextNode)
            <$> (noneOf "\n") `someTill` lookAhead newline
          <|> pure mempty

  codeNode :: Parser Node
  codeNode = L.indentBlock scn $ do
    code <- embeddedCode
    return $ L.IndentMany Nothing (pure . EmbeddedCodeNode code . Tree) node

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
    textIndent <- (\leadingNewlines spaces ->
      if leadingNewlines
        then spaces
        else (spaces - 1) + fromIntegral (unPos separatorIndent))
      <$> ((> 0) . length <$> many newline)
      <*> (length <$> many spaceOrTab)
    firstLine <- anyChar `manyTill` newline
    indentedLines <- splitOn "\n"
      <$> anyChar `manyTill` try (L.indentGuard scn LT separatorIndent)
    return $ firstLine ++ (drop textIndent =<< indentedLines)

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

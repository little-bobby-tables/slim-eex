{-# LANGUAGE TypeFamilies #-}

module Parser where
  import Control.Applicative (empty)
  import Control.Monad (void)
  import Text.Megaparsec
  import Text.Megaparsec.String
  import qualified Text.Megaparsec.Lexer as L

  newtype Tree = Tree [Node] deriving (Eq, Show)

  data Node = Node String [Attr] Tree
            | EmbeddedCodeNode EmbeddedCode Tree
    deriving (Eq, Show)

  newtype Attr = Attr (String, String) deriving (Eq, Show)

  data EmbeddedCode = ControlCode String
                    | EscapedCode String
                    | UnescapedCode String
    deriving (Eq, Show)

  slim :: Parser Tree
  slim = tree <* eof

  tree :: Parser Tree
  tree = do
    nodes <- many $ L.nonIndented scn node
    return $ Tree nodes

  node :: Parser Node
  node = codeNode <|> htmlNode

  htmlNode :: Parser Node
  htmlNode = L.indentBlock scn $ do
    name <- htmlEntityName
    shorthandAttrs <- many (dotClass <|> hashId)
    attrs <- (shorthandAttrs ++) <$> attribute `sepBy` spaceOrTab
    return $ L.IndentMany Nothing (return . (Node name attrs) . Tree) node

  codeNode :: Parser Node
  codeNode = L.indentBlock scn $ do
    code <- embeddedCode
    return $ L.IndentMany Nothing (return . (EmbeddedCodeNode code) . Tree) node

  embeddedCode :: Parser EmbeddedCode
  embeddedCode = control <|> unescaped <|> escaped
    where
      control :: Parser EmbeddedCode
      control = char '-' >> code >>= return . ControlCode
      unescaped :: Parser EmbeddedCode
      unescaped = string "==" >> code >>= return . UnescapedCode
      escaped :: Parser EmbeddedCode
      escaped = char '=' >> code >>= return . EscapedCode
      code :: Parser String
      code = spaceOrTab >> anyChar `manyTill` (lookAhead newline)

  attribute :: Parser Attr
  attribute = do
    name <- htmlEntityName
    _ <- char '='
    content <- quotedString
    return $ Attr (name, content)

  dotClass :: Parser Attr
  dotClass = do
    _ <- char '.'
    className <- htmlEntityName
    return $ Attr ("class", className)

  hashId :: Parser Attr
  hashId = do
    _ <- char '#'
    idValue <- htmlEntityName
    return $ Attr ("id", idValue)

  htmlEntityName :: Parser String
  htmlEntityName = lexeme $ some (alphaNumChar <|> char '_' <|> char '-')

  quotedString :: Parser String
  quotedString = do
    _ <- char '"'
    str <- many quotedChar
    _ <- char '"'
    return str
    where
      quotedChar :: Parser Char
      quotedChar = unescaped <|> escaped
      unescaped :: Parser Char
      unescaped = noneOf "\\\""
      escaped :: Parser Char
      escaped = char '\\' >> oneOf "\\\""

  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme sc

  sc :: Parser ()
  sc = L.space (void $ spaceOrTab) empty empty

  scn :: Parser ()
  scn = L.space (void spaceChar) empty empty

  spaceOrTab :: Parser Char
  spaceOrTab = oneOf " \t"

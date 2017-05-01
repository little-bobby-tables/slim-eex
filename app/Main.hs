{-# LANGUAGE TypeFamilies #-}

module Main where
  import Control.Applicative (empty)
  import Control.Monad (void)
  import Text.Megaparsec
  import Text.Megaparsec.String
  import qualified Text.Megaparsec.Lexer as L
  import Text.Megaparsec.Prim (MonadParsec)

  newtype Tree = Tree [Node] deriving (Show)

  data Node = Node String [Attr] Tree deriving (Show)

  newtype Attr = Attr (String, String) deriving (Show)

  slim :: Parser Tree
  slim = tree <* eof

  tree :: Parser Tree
  tree = do
    nodes <- many $ L.nonIndented scn node
    return $ Tree nodes

  node :: Parser Node
  node = L.indentBlock scn $ do
    name <- tagName
    --attributes <- attribute `sepBy` spaceChar
    return $ L.IndentMany Nothing (return . (Node name []) . Tree) node

  attribute :: Parser Attr
  attribute = return $ Attr ("", "")

  tagName :: Parser String
  tagName = lexeme $ some (alphaNumChar <|> char '_' <|> char '-')

  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme sc

  sc :: Parser ()
  sc = L.space (void $ spaceOrTab) empty empty

  scn :: Parser ()
  scn = L.space (void spaceChar) empty empty

  spaceOrTab :: (MonadParsec e s m, Token s ~ Char) => m Char
  spaceOrTab = oneOf " \t"

  main :: IO ()
  main = do
    let source = "\
    \head \n\
    \  title \n\
    \    nested \n\
    \  meta \n\
    \body \n\
    \  div \n\
    \"
    parseTest slim source

{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Parser.Internal
  (textBlock, quotedString, restOfLine,
   nonIndented, indentBlock, manyIndented,
   strippedOff, whitespaceLength, indentedBy,
   symbol, lexeme, spaceOrTab) where

  import Control.Applicative (empty)
  import Control.Monad (void)
  import Text.Megaparsec
  import Text.Megaparsec.String
  import qualified Text.Megaparsec.Lexer as L

  import Data.List.Split (split, splitOn, keepDelimsR, onSublist)

  -- Text blocks begin with a separator (e.g. |) and consume
  -- everything indented deeper than the separator. The distance
  -- between the first character of the first line and the beginning
  -- of subsequent lines indicates spacing.
  --
  -- | First line
  --   Between "line" and "Between", there will be no spaces
  --        While between "spaces" and "While" there will be five.
  textBlock :: String -> Parser String
  textBlock separator = do
    separatorIndent <- string separator *> L.indentLevel
    textIndent <- leadingNewlines *> whitespaceLength
              <|> addIndent separatorIndent <$> whitespaceLength
    (++) <$> anyChar `manyTill` newline
         <*> (anyChar `indentedBy` separatorIndent)
             `inlinedAndStrippedOff` textIndent
    where
      leadingNewlines = some newline
      addIndent = (+) . subtract 1 . fromIntegral . unPos

  inlinedAndStrippedOff :: Parser String -> Int -> Parser String
  inlinedAndStrippedOff text whitespace =
    (drop whitespace =<<) <$> splitOn "\n" <$> text

  strippedOff :: Parser String -> Int -> Parser String
  strippedOff text whitespace =
    (drop whitespace =<<) <$> (split . keepDelimsR . onSublist) "\n" <$> text

  whitespaceLength :: Parser Int
  whitespaceLength = length <$> many spaceOrTab

  indentedBy :: Parser a -> Pos -> Parser [a]
  indentedBy a = (a `manyTill`) . try . (L.indentGuard scn LT)

  -- A quoted string (without quotes), which may contain escape
  -- sequences (e.g. "\"string\"").
  quotedString :: Parser String
  quotedString = lexeme $ char '"' *> many quotedChar <* char '"'
    where
      quotedChar :: Parser Char =
        unescaped <|> escaped
      unescaped = noneOf "\\\""
      escaped = char '\\' *> oneOf "\\\""

  restOfLine :: Parser String
  restOfLine = optional spaceOrTab *> anyChar `manyTill` lookAhead newline

  nonIndented :: Parser a -> Parser a
  nonIndented = L.nonIndented scn

  indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
  indentBlock = L.indentBlock scn

  manyIndented :: ([b] -> Parser a) -> Parser b -> L.IndentOpt Parser a b
  manyIndented = L.IndentMany Nothing

  -- Lex helpers

  symbol :: String -> Parser String
  symbol = L.symbol sc

  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme sc

  -- Space consumers

  sc :: Parser ()
  sc = L.space (void spaceOrTab) empty empty

  scn :: Parser ()
  scn = L.space (void spaceChar) empty empty

  spaceOrTab :: Parser Char
  spaceOrTab = oneOf " \t"

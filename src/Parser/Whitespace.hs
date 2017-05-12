{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module Parser.Whitespace where
  import Parser.Types

  import Text.Megaparsec
  import Text.Megaparsec.String

  data Whitespace = Leading
                  | Trailing
                  | LeadingAndTrailing
                  | NoWhitespace
    deriving (Eq, Show)

  slimTree :: [SlimNode] -> Tree
  slimTree = Tree . concatSlimNodes

  concatSlimNodes :: [SlimNode] -> [Node]
  concatSlimNodes ((SlimNode innerNs):ns) = innerNs ++ (concatSlimNodes ns)
  concatSlimNodes [] = []

  slimWhitespace :: Parser Whitespace
  slimWhitespace =
    (string "<>" *> pure LeadingAndTrailing)
    <|> (char '<' *> pure Leading)
    <|> (char '>' *> pure Trailing)
    <|> pure NoWhitespace

  slimNode :: Whitespace -> Node -> SlimNode
  slimNode LeadingAndTrailing node = SlimNode
    [ WhitespaceNode
    , node
    , WhitespaceNode ]
  slimNode Leading node = SlimNode
    [ WhitespaceNode
    , node ]
  slimNode Trailing node = SlimNode
    [ node
    , WhitespaceNode ]
  slimNode NoWhitespace node = SlimNode
    [ node ]

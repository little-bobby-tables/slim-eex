module Parser.Types where

  newtype Tree = Tree [Node] deriving (Eq, Show)

  data Node = HtmlNode String [Attr] Tree
            | EmbeddedCodeNode EmbeddedCode Tree
            | EmbeddedEngineNode String String
            | VerbatimTextNode String
            | CommentNode Comment
            | WhitespaceNode
    deriving (Eq, Show)

  {-|
    Functions that parse individual nodes in fact return a list:

    [ (optional) WhitespaceNode <-- leading whitespace (div<)
    , parsed node
    , (optional) WhitespaceNode ] <-- trailing whitespace (div>)

    The list is wrapped into the 'SlimNode' type to avoid confusion
    with functions that produce multiple nodes in one pass.

    See "Parser.Whitespace" for helpers that operate on this type.
  -}
  newtype SlimNode = SlimNode [Node] deriving (Eq, Show)

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

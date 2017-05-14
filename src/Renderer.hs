module Renderer (renderTree) where
    import Parser

    import Data.List (intercalate)
    import Data.Char (toLower)

    renderTree :: Tree -> String
    renderTree (Tree nodes) = render =<< nodes

    renderAttr :: Attr -> String
    renderAttr (EscapedCodeAttr name code) =
      name ++ "=<%= " ++ code ++ " %>"
    renderAttr (UnescapedCodeAttr name code) =
      name ++ "=<%= safe(" ++ code ++ ") %>"

    isVoidTag :: String -> Bool
    isVoidTag = (flip elem) [
      "area", "base", "br", "col", "embed", "hr", "img", "input",
      "keygen", "link", "meta", "param", "source", "track", "wbr"]

    render :: Node -> String
    render (HtmlNode name attrs tree) =
      let element = toLower <$> name
          tagLine = intercalate " " (element : (renderAttr <$> attrs))
      in if isVoidTag element
        then "<" ++ tagLine ++ "/>"
        else "<" ++ tagLine ++ ">" ++ renderTree tree ++ "</" ++ element ++ ">"
    render (EmbeddedCodeNode (ControlCode c) tree) =
      "<% " ++ c ++ " %>" ++ renderTree tree
    render (EmbeddedCodeNode (EscapedCode c) tree) =
      "<%= " ++ c ++ " %>" ++ renderTree tree
    render (EmbeddedCodeNode (UnescapedCode c) tree) =
      "<%= safe(" ++ c ++ ") %>" ++ renderTree tree
    render (VerbatimTextNode text) =
      text
    render (CommentNode (HtmlComment comment)) =
      "<!--" ++ comment ++ "-->"
    render (WhitespaceNode) =
      " "

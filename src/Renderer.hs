{-# LANGUAGE LambdaCase #-}

module Renderer (renderTree) where
  import Parser

  import Data.List (intercalate)
  import Data.List.Split (splitOn)
  import Data.Char (toLower)

  renderTree :: Tree -> String
  renderTree (Tree nodes) = render =<< nodes

  isVoidTag :: String -> Bool
  isVoidTag = (flip elem) [
    "area", "base", "br", "col", "embed", "hr", "img", "input",
    "keygen", "link", "meta", "param", "source", "track", "wbr"]

  render :: Node -> String
  render (HtmlNode name attrs tree) =
    let element = toLower <$> name
        tagLine = intercalate " " (element : (renderAttrs attrs))
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

  renderAttrs :: [Attr] -> [String]
  renderAttrs = renderFold . foldAttrs
    where
      foldAttrs :: [Attr] -> ([String], [String], [String])
      foldAttrs = foldr (\a (cls, ids, attrs) ->
          let attr = renderAttr a
          in case attr of
            ("class", c) -> (c : cls, ids, attrs)
            ("id", i) -> (cls, i : ids, attrs)
            (k, "") -> (cls, ids, k : attrs)
            (k, v) -> (cls, ids, (k ++ "=\"" ++ v ++ "\"") : attrs)
        ) ([], [], [])
      renderFold :: ([String], [String], [String]) -> [String]
      renderFold (cls, ids, attrs) = filter (/= []) (
          (if (not . null) cls
              then "class=\"" ++ intercalate " " cls ++ "\"" else "")
        : (if (not . null) ids
              then "id=\"" ++ intercalate "-" ids ++ "\"" else "")
        : attrs)
      renderAttr :: Attr -> (String, String)
      renderAttr (EscapedCodeAttr name code) =
        (name, "<%= " ++ code ++ " %>")
      renderAttr (UnescapedCodeAttr name code) =
        (name, "<%= safe(" ++ code ++ ") %>")
      renderAttr (EscapedAttr name value) =
        (name, escapeQuoted value)
      renderAttr (UnescapedAttr name value) =
        (name, "<%= safe(" ++ escapeQuoted value ++ ") %>")
      renderAttr (BooleanAttr name) =
        (name, "")

  escapeQuoted :: String -> String
  escapeQuoted = intercalate "\\\"" . splitOn "\""

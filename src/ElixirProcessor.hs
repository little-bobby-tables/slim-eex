module ElixirProcessor where
  import Parser

  import Data.List (isInfixOf, isSuffixOf)

  process :: Tree -> Tree
  process (Tree ns) = Tree (processNodes (processChildTree <$> ns))
    where
      processChildTree :: Node -> Node
      processChildTree (HtmlNode n a t) = HtmlNode n a (process t)
      processChildTree (EmbeddedCodeNode c t) = EmbeddedCodeNode c (process t)
      processChildTree n = id n

  processNodes :: [Node] -> [Node]
  processNodes (codeNode@(EmbeddedCodeNode code _) : rest) =
    case rest of
      (elseNode@(EmbeddedCodeNode (ControlCode "else") _) : rest') ->
        codeNode : elseNode : endNode : (processNodes rest')
      _ ->
        if isDoEndBlock code
          then codeNode : endNode : (processNodes rest)
          else codeNode : (processNodes rest)
  processNodes (n : ns) = n : (processNodes ns)
  processNodes [] = []

  endNode :: Node
  endNode = EmbeddedCodeNode (ControlCode "end") (Tree [])

  isCodeNode :: Node -> Bool
  isCodeNode (EmbeddedCodeNode _ _) = True
  isCodeNode _ = False

  isDoEndBlock :: EmbeddedCode -> Bool
  isDoEndBlock = matchCode (\c ->
    ("->" `isSuffixOf` c && "fn" `isInfixOf` c)
    || ("do" `isSuffixOf` c))

  matchCode :: (String -> Bool) -> EmbeddedCode -> Bool
  matchCode p (ControlCode c) = p c
  matchCode p (EscapedCode c) = p c
  matchCode p (UnescapedCode c) = p c

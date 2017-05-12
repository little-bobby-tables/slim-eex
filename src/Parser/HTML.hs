module Parser.HTML where
  import Parser.Internal

  import Text.Megaparsec
  import Text.Megaparsec.String

  {-|
    Name     ::= (Letter | '_' | ':') (NameChar)*
    NameChar	::= Letter | Digit | '.' | '-' | '_' | ':'

    Note that we can't use dots in element names;
    they're reserved for the attribute shorthand notation.
  -}
  elementName :: Parser String
  elementName = lexeme $
    (:) <$> (letterChar <|> oneOf "_:")
        <*> many (alphaNumChar <|> oneOf "-_:")

  attributeName :: Parser String
  attributeName = lexeme $ some (alphaNumChar <|> oneOf "@-_:")

  shorthandName :: Parser String
  shorthandName = lexeme $ some (alphaNumChar <|> oneOf "_-")

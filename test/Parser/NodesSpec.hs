{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parser.NodesSpec where
  import Parser (Tree(..), Node(..), Attr(..), EmbeddedCode(..), slim)

  import Test.Hspec
  import Test.Hspec.Megaparsec

  import Text.Megaparsec (parse)

  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "nodes" $ do
      it "may be nested" $ do
        parse slim "<source>" (unpack [text|
        head
          title
            nested
          meta
        body
          div
        |]) `shouldParse`
          Tree [
            Node "head" [] (Tree [
              Node "title" [] (Tree [
                Node "nested" [] (Tree [])
              ])
            , Node "meta" [] (Tree [])
            ])
          , Node "body" [] (Tree [
              Node "div" [] (Tree [])
            ])
          ]

    describe "inline content" $ do
      it "may be text" $ do
        parse slim "<source>" (unpack [text|
        p Come along
          div.child
        div.class attr="attr" with me
        |]) `shouldParse`
          Tree [
            Node "p" [] (Tree [
              VerbatimTextNode "Come along"
            , Node "div" [Attr ("class", "child")] (Tree [])
            ])
          , Node "div" [
              Attr ("class", "class")
            , Attr ("attr", "attr")] (Tree [
              VerbatimTextNode "with me"
            ])
          ]

      it "may be embedded code" $ do
        parse slim "<source>" (unpack [text|
        p == @unescaped
          span=@text
        |]) `shouldParse`
          Tree [
            Node "p" [] (Tree [
              EmbeddedCodeNode (UnescapedCode "@unescaped") (Tree [])
            , Node "span" [] (Tree [
                EmbeddedCodeNode (EscapedCode "@text") (Tree [])
              ])
            ])
          ]

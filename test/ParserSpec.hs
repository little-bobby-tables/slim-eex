 {-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module ParserSpec where
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
      it "parses nested nodes" $ do
        parse slim "<source>" (unpack [text|
        head
          title
            nested
          meta
        body
          div
        |]) `shouldParse`
          Tree [
            Node "head" [] $
              Tree [Node "title" [] $
                Tree [Node "nested" [] $ Tree []]
                  , Node "meta" [] $ Tree []]
          , Node "body" [] $
              Tree [Node "div" [] $ Tree []]
          ]

      it "parses string attributes" $ do
        parse slim "<source>" (unpack [text|
        div data-source="some source"
          span data-escaped="has \"escape \\sequences\"" data-also-this="yo"
        |]) `shouldParse`
          Tree [
            Node "div" [
              Attr ("data-source", "some source")] $
              Tree [Node "span" [
                Attr ("data-escaped", "has \"escape \\sequences\"")
              , Attr ("data-also-this", "yo")] $ Tree []]
          ]

      it "parses dot and hash notations for attributes" $ do
        parse slim "<source>" (unpack [text|
        div.1_class.another-class class="yet-another-class"
        span#some_id1#someid2 id="some-id3"
        header#combined.notation#is-also.fine class="definitely"
        |]) `shouldParse`
          Tree [
            Node "div" [
              Attr ("class", "1_class")
            , Attr ("class", "another-class")
            , Attr ("class", "yet-another-class")] $ Tree []
          , Node "span" [
              Attr ("id", "some_id1")
            , Attr ("id", "someid2")
            , Attr ("id", "some-id3")] $ Tree []
          , Node "header" [
              Attr ("id", "combined")
            , Attr ("class", "notation")
            , Attr ("id", "is-also")
            , Attr ("class", "fine")
            , Attr ("class", "definitely")] $ Tree []
          ]

    describe "embedded code" $ do
      it "parses embedded code" $ do

        parse slim "<source>" (unpack [text|
        - cond = true
        = if cond do
          div data-cond="true"
            == "<script>unescaped()</script>"
        - else
          div
            oops
        |]) `shouldParse`
          Tree [
            EmbeddedCodeNode
              (ControlCode "cond = true") (Tree [])
          , EmbeddedCodeNode
              (EscapedCode "if cond do") (Tree [
                Node "div" [Attr ("data-cond", "true")]
                  (Tree [
                    EmbeddedCodeNode
                      (UnescapedCode "\"<script>unescaped()</script>\"") (Tree [])
                  ])
              ])
          , EmbeddedCodeNode
              (ControlCode "else") (Tree [
                Node "div" [] (Tree [
                  Node "oops" [] (Tree [])
                ])
              ])
          ]

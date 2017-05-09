{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parser.AttributesSpec where
  import Parser (Tree(..), Node(..), Attr(..), slim)

  import Test.Hspec
  import Test.Hspec.Megaparsec

  import Text.Megaparsec (parse)

  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "naming" $ do
      it "may contain a number of special characters" $ do
        parse slim "<source>" (unpack [text|
        button v-on:click="myfunc"
        button(@click="myfunc")
        |]) `shouldParse`
          Tree [
            Node "button" [
              EscapedAttr "v-on:click" "myfunc"] (Tree [])
          , Node "button" [
              EscapedAttr "@click" "myfunc"] (Tree [])
          ]

    describe "string attributes" $ do
      it "may contain escape sequences" $ do
        parse slim "<source>" (unpack [text|
        div data-source="some source"
          span data-escaped="has \"escape \\sequences\"" data-also-this="yo"
        random
        |]) `shouldParse`
          Tree [
            Node "div" [
              EscapedAttr "data-source" "some source"] (Tree [
                Node "span" [
                  EscapedAttr "data-escaped" "has \"escape \\sequences\""
                , EscapedAttr "data-also-this" "yo"] (Tree [])])
          , Node "random" [] (Tree [])
          ]

      it "may be explicitly set to be unesaped" $ do
        parse slim "<source>" (unpack [text|
        div am=="not escaped" also-not=="escaped"
        |]) `shouldParse`
          Tree [
            Node "div" [
              UnescapedAttr "am" "not escaped"
            , UnescapedAttr "also-not" "escaped"] (Tree [])
          ]

    describe "code attributes" $ do
      it "may contain spaces inside balanced delimiters" $ do
        parse slim "<source>" (unpack [text|
        div data-source=render(value(x, (y && z)) || another_value "h")
        div ( data-something=(pseudo)[code]{that}(should be )%{ valid} )
        |]) `shouldParse`
          Tree [
            Node "div" [EscapedCodeAttr "data-source"
              "render(value(x, (y && z)) || another_value \"h\")"] (Tree [])
          , Node "div" [EscapedCodeAttr "data-something"
            "(pseudo)[code]{that}(should be )%{ valid}"] (Tree [])
          ]

      it "may be explicitly set to be unesaped" $ do
        parse slim "<source>" (unpack [text|
        div e==render(value(x))
        div{data-something==%{:a => 1, 2 => :b}}
        |]) `shouldParse`
          Tree [
            Node "div" [UnescapedCodeAttr "e" "render(value(x))"] (Tree [])
          , Node "div" [UnescapedCodeAttr "data-something"
            "%{:a => 1, 2 => :b}"] (Tree [])
          ]

    describe "attribute wrapping" $ do
      it "allows attributes to be spread across multiple lines" $ do
        parse slim "<source>" (unpack [text|
        div[attr="value" another="value"
          yet-another="value"
            and-another="value"]
          div.child
        |]) `shouldParse`
          Tree [Node "div" [
                  EscapedAttr "attr" "value"
                , EscapedAttr "another" "value"
                , EscapedAttr "yet-another" "value"
                , EscapedAttr "and-another" "value"] (Tree [
                    Node "div" [EscapedAttr "class" "child"] (Tree [])
                ])]

      it "permits attributes without values" $ do
        parse slim "<source>" (unpack [text|
        div{with="value" without value also-with="value"}
          div.child
        |]) `shouldParse`
          Tree [Node "div" [
                  EscapedAttr "with" "value"
                , BooleanAttr "without"
                , BooleanAttr "value"
                , EscapedAttr "also-with" "value"] (Tree [
                    Node "div" [EscapedAttr "class" "child"] (Tree [])
                ])]

      it "handles redundant whitespace" $ do
        parse slim "<source>" (unpack [text|
        div ( attr="value"
          another="value" )
          div.child
        |]) `shouldParse`
          Tree [Node "div" [
                  EscapedAttr "attr" "value"
                , EscapedAttr "another" "value"] (Tree [
                    Node "div" [EscapedAttr "class" "child"] (Tree [])
                ])]

    describe "attribute shorthand parsing" $ do
      it "converts .dot notation to class names" $ do
        parse slim "<source>" (unpack [text|
        div.1_class.another-class class="yet-another-class"
        |]) `shouldParse`
          Tree [Node "div" [
                 EscapedAttr "class" "1_class"
               , EscapedAttr "class" "another-class"
               , EscapedAttr "class" "yet-another-class"] (Tree [])]

    it "converts #hash notation to element ids" $ do
      parse slim "<source>" (unpack [text|
      span#some_id1#someid2 id="some-id3"
      |]) `shouldParse`
        Tree [Node "span" [
                EscapedAttr "id" "some_id1"
              , EscapedAttr "id" "someid2"
              , EscapedAttr "id" "some-id3"] (Tree [])]

    it "allows both notations to be combined" $ do
      parse slim "<source>" (unpack [text|
      header#combined.notation#is-also.fine class="definitely"
      |]) `shouldParse`
        Tree [Node "header" [
                EscapedAttr "id" "combined"
              , EscapedAttr "class" "notation"
              , EscapedAttr "id" "is-also"
              , EscapedAttr "class" "fine"
              , EscapedAttr "class" "definitely"] (Tree [])]

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parser.InlineContentSpec where
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
    describe "inline content" $ do
      it "may be text" $ do
        parse slim "<source>" (unpack [text|
        p Come along
          div.child
        div.class attr="attr" with me
        |]) `shouldParse`
          Tree [
            HtmlNode "p" [] (Tree [
              VerbatimTextNode "Come along"
            , HtmlNode "div" [EscapedAttr "class" "child"] (Tree [])
            ])
          , HtmlNode "div" [
              EscapedAttr "class" "class"
            , EscapedAttr "attr" "attr"] (Tree [
                VerbatimTextNode "with me"
            ])
          ]

      it "may be embedded code" $ do
        parse slim "<source>" (unpack [text|
        p == @unescaped
          span=@text
        |]) `shouldParse`
          Tree [
            HtmlNode "p" [] (Tree [
              EmbeddedCodeNode (UnescapedCode "@unescaped") (Tree [])
            , HtmlNode "span" [] (Tree [
                EmbeddedCodeNode (EscapedCode "@text") (Tree [])
              ])
            ])
          ]

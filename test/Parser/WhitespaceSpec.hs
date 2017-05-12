{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parser.WhitespaceSpec where
  import Parser
    (Tree(..), Node(..), Attr(..), EmbeddedCode(..), slim)

  import Test.Hspec
  import Test.Hspec.Megaparsec

  import Text.Megaparsec (parse)

  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe ">, <, and <>" $ do
      it "produces whitespace nodes" $ do
        parse slim "<source>" (unpack [text|
        div<
          p>
            a<>
          p<>
        |]) `shouldParse`
          Tree [
            WhitespaceNode
          , Node "div" [] (Tree [
              Node "p" [] (Tree [
                WhitespaceNode
              , Node "a" [] (Tree [])
              , WhitespaceNode
              ])
            , WhitespaceNode
            , WhitespaceNode
            , Node "p" [] (Tree [])
            , WhitespaceNode
            ])
          ]

    describe "embedded code whitespace" $ do
      it "may be applied inline" $ do
        parse slim "<source>" (unpack [text|
        div =< hey("there")
        |]) `shouldParse`
          Tree [
            Node "div" [] (Tree [
              WhitespaceNode
            , EmbeddedCodeNode (EscapedCode "hey(\"there\")") (Tree [])
            ])
          ]

      it "may be applied to unescaped code" $ do
        parse slim "<source>" (unpack [text|
        ==<> hey("there")
        |]) `shouldParse`
          Tree [
            WhitespaceNode
          , EmbeddedCodeNode (UnescapedCode "hey(\"there\")") (Tree [])
          , WhitespaceNode
          ]

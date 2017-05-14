{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module ElixirProcessorSpec where
  import ElixirProcessor
  import Parser (Tree(..), Node(..), EmbeddedCode(..), slim)

  import Test.Hspec
  import Test.Hspec.Megaparsec

  import Text.Megaparsec (parse)

  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "embedded code block processing" $ do
      it "inserts <end> tokens for anonymous functions" $ do
        (process <$> (parse slim "<source>" (unpack [text|
        div
          = Enum.map ["hey", "there"], fn muh_text ->
            p = muh_text
        |]))) `shouldParse`
          Tree [
            HtmlNode "div" [] (Tree [
              EmbeddedCodeNode
                (EscapedCode "Enum.map [\"hey\", \"there\"], fn muh_text ->")
                (Tree [
                  HtmlNode "p" [] (Tree [
                    EmbeddedCodeNode (EscapedCode "muh_text") (Tree [])
                  ])
                ])
            , EmbeddedCodeNode (ControlCode "end") (Tree [])
            ])
          ]

      it "inserts <end> tokens for <if> blocks" $ do
        (process <$> (parse slim "<source>" (unpack [text|
        div
          = if some_condition do
            | Hey
          - else
            = if nested_condition do
              | Even unidiomatic Elixir
            - else
              | Should render properly
          = if another_condition do
            | This is the last one, I promise
        |]))) `shouldParse`
          Tree [
            HtmlNode "div" [] (Tree [
              EmbeddedCodeNode (EscapedCode "if some_condition do")
                (Tree [VerbatimTextNode "Hey"])
            , EmbeddedCodeNode (ControlCode "else")
                (Tree [
                  EmbeddedCodeNode (EscapedCode "if nested_condition do")
                    (Tree [VerbatimTextNode "Even unidiomatic Elixir"])
                , EmbeddedCodeNode (ControlCode "else")
                    (Tree [VerbatimTextNode "Should render properly"])
                , EmbeddedCodeNode (ControlCode "end")
                    (Tree [])
                ])
            , EmbeddedCodeNode (ControlCode "end")
                (Tree [])
            , EmbeddedCodeNode (EscapedCode "if another_condition do")
                (Tree [VerbatimTextNode "This is the last one, I promise"])
            , EmbeddedCodeNode (ControlCode "end")
                (Tree [])
            ])
          ]

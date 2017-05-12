{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parser.TextSpec where
  import Parser (Tree(..), Node(..), slim)

  import Test.Hspec
  import Test.Hspec.Megaparsec

  import Text.Megaparsec (parse)

  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "verbatim text node - |" $ do
      it "converts indentation to spaces" $ do
        parse slim "<source>" (unpack [text|
        p
          | Head
            No spaces in front of the line.
             One space in front it.
              Two spaces in front of it.
                 Five spaces in front of it.
             One space.
        p
          |Head
           No spaces.
            One space.
        p
          |  Head
              One space.
        |]) `shouldParse`
          Tree [
            HtmlNode "p" [] (Tree [
              VerbatimTextNode "HeadNo spaces in front of the line\
                \. One space in front it.  Two spaces in front of it\
                \.     Five spaces in front of it\
                \. One space."
            ])
          , HtmlNode "p" [] (Tree [
              VerbatimTextNode "HeadNo spaces. One space."
            ])
          , HtmlNode "p" [] (Tree [
              VerbatimTextNode "Head One space."
            ])
          ]

      it "accepts text starting on separate lines" $ do
        parse slim "<source>" (unpack [text|
        p
          | No spaces.
          |
            No spaces.
             One space.
          |


            No spaces.
              Two spaces.
        |]) `shouldParse`
          Tree [
            HtmlNode "p" [] (Tree [
              VerbatimTextNode "No spaces."
            , VerbatimTextNode "No spaces. One space."
            , VerbatimTextNode "No spaces.  Two spaces."
            ])
          ]

    describe "verbatim text node - '" $ do
      it "appends trailing whitespace" $ do
        parse slim "<source>" (unpack [text|
        p
          ' H
          | ey
        |]) `shouldParse`
          Tree [
            HtmlNode "p" [] (Tree [
              VerbatimTextNode "H"
            , WhitespaceNode
            , VerbatimTextNode "ey"
            ])
          ]

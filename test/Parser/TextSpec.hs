{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parser.TextSpec where
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
    describe "text content" $ do
      it "can begin on the same line as node definition" $ do
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
            Node "p" [] (Tree [
              VerbatimTextNode "HeadNo spaces in front of the line\
                \. One space in front it.  Two spaces in front of it\
                \.     Five spaces in front of it\
                \. One space."
            ])
          , Node "p" [] (Tree [
              VerbatimTextNode "HeadNo spaces. One space."
            ])
          , Node "p" [] (Tree [
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
            Node "p" [] (Tree [
              VerbatimTextNode "No spaces."
            , VerbatimTextNode "No spaces. One space."
            , VerbatimTextNode "No spaces.  Two spaces."
            ])
          ]

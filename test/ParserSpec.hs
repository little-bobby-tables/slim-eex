 {-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module ParserSpec where
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
    describe "nested nodes" $ do
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

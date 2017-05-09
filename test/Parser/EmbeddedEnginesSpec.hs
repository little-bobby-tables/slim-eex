{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parser.EmbeddedEnginesSpec where
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
    describe "embedded engine" $ do
      it "consumes everything indented further than the header" $ do
        parse slim "<source>" (unpack [text|
        div
          javascript:
            alert("hello there");
            const chopper = {
              owner: 'me'
            };
        |]) `shouldParse`
          Tree [
            Node "div" [] (Tree [
              EmbeddedEngineNode "javascript"
              "alert(\"hello there\");\n\
              \const chopper = {\n\
              \  owner: 'me'\n\
              \};"
            ])
          ]

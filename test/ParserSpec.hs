module ParserSpec where
  import Parser (Tree(..), Node(..), slim)

  import Test.Hspec
  import Test.Hspec.Megaparsec

  import Text.Megaparsec (parse)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "nested nodes" $ do
      it "parses nested nodes" $ do
        parse slim "<source>" "\
        \head\n\
        \  title \n\
        \    nested \n\
        \  meta \n\
        \body \n\
        \  div\n\
        \" `shouldParse` Tree [
            Node "head" [] $
              Tree [Node "title" [] $
                Tree [Node "nested" [] $ Tree []]
                  , Node "meta" [] $ Tree []]
          , Node "body" [] $
              Tree [Node "div" [] $ Tree []]
          ]

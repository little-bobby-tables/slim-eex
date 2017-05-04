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
   describe "string attributes" $ do
     it "may contain escape sequences" $ do
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

   describe "attribute shorthand parsing" $ do
     it "converts .dot notation to class names" $ do
       parse slim "<source>" (unpack [text|
       div.1_class.another-class class="yet-another-class"
       |]) `shouldParse`
         Tree [Node "div" [
                 Attr ("class", "1_class")
               , Attr ("class", "another-class")
               , Attr ("class", "yet-another-class")] (Tree [])]

     it "converts #hash notation to element ids" $ do
       parse slim "<source>" (unpack [text|
       span#some_id1#someid2 id="some-id3"
       |]) `shouldParse`
         Tree [Node "span" [
                 Attr ("id", "some_id1")
               , Attr ("id", "someid2")
               , Attr ("id", "some-id3")] (Tree [])]

     it "allows both notations to be combined" $ do
       parse slim "<source>" (unpack [text|
       header#combined.notation#is-also.fine class="definitely"
       |]) `shouldParse`
         Tree [Node "header" [
                 Attr ("id", "combined")
               , Attr ("class", "notation")
               , Attr ("id", "is-also")
               , Attr ("class", "fine")
               , Attr ("class", "definitely")] (Tree [])]

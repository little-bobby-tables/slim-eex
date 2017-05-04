{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parser.EmbeddedCodeSpec where
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
   describe "embedded code" $ do
     it "may contain other nodes" $ do
       parse slim "<source>" (unpack [text|
       - cond = true
       = if cond do
         div data-cond="true"
           == "<script>unescaped()</script>"
       - else
         div
           oops
       |]) `shouldParse`
         Tree [
           EmbeddedCodeNode
             (ControlCode "cond = true") (Tree [])
         , EmbeddedCodeNode
             (EscapedCode "if cond do") (Tree [
               Node "div" [Attr ("data-cond", "true")]
                 (Tree [
                   EmbeddedCodeNode
                     (UnescapedCode "\"<script>unescaped()</script>\"") (Tree [])
                 ])
             ])
         , EmbeddedCodeNode
             (ControlCode "else") (Tree [
               Node "div" [] (Tree [
                 Node "oops" [] (Tree [])
               ])
             ])
         ]

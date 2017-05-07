{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Parser.CommentsSpec where
  import Parser (Tree(..), Node(..), Attr(..), Comment(..), slim)

  import Test.Hspec
  import Test.Hspec.Megaparsec

  import Text.Megaparsec (parse)

  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "html comment node - \\!" $ do
      it "has similar semantics to other text blocks" $ do
        parse slim "<source>" (unpack [text|
        \! HTML comments
            have similar semantics to other text blocks:
              they support nesting, with indentation being converted to spaces
        div
          \!You can
             start them
          \!
            On
              another line
        |]) `shouldParse`
          Tree [
            CommentNode (HtmlComment "HTML comments\
            \ have similar semantics to other text blocks:   they\
            \ support nesting, with indentation being converted to spaces")
          , Node "div" [] (Tree [
              CommentNode (HtmlComment "You can start them")
            , CommentNode (HtmlComment "On  another line")
            ])
          ]

    describe "slim comment node - \\" $ do
      it "has similar semantics to other text blocks" $ do
        parse slim "<source>" (unpack [text|
        \ Comment
          s
        \ Comment
           Indented comment
             div.i-dont-need-this
               | I don't need this either
        div.uncommented
          \Commen
           t
        |]) `shouldParse`
          Tree [
            CommentNode (SlimComment "Comments")
          , CommentNode (SlimComment "Comment Indented comment\
            \   div.i-dont-need-this     | I don't need this either")
          , Node "div" [Attr ("class", "uncommented")] (Tree [
              CommentNode (SlimComment "Comment")
            ])
          ]

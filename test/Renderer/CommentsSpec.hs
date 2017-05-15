{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Renderer.CommentsSpec where
  import Driver

  import Test.Hspec

  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "comments" $ do
      it "skips slim comments" $ do
        slimToEex (unpack [text|
        \ Comment
        div
          \ Nested
            comment
          p
        \ The last one
        |]) `shouldBe` (((filter (/= '\n')) . unpack) [text|
        <div>
        <p></p>
        </div>
        |])

      it "renders html comments" $ do
        slimToEex (unpack [text|
        \! HTML comment
        div
          \! This
              should be included in the EEx output
          p
        |]) `shouldBe` (((filter (/= '\n')) . unpack) [text|
        <!--HTML comment-->
        <div>
        <!--This should be included in the EEx output-->
        <p></p>
        </div>
        |])

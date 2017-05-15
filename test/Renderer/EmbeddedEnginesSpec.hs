{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Renderer.EmbeddedEnginesSpec where
  import Driver

  import Test.Hspec

  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "embedded engines" $ do
      it "renders javascript" $ do
        slimToEex (unpack [text|
        javascript:
          for (var i = 0; i < good.ol.for.loop.length; i++) {
            loop(i);
          }
        |]) `shouldBe` ((init . unpack) [text|
        <script>for (var i = 0; i < good.ol.for.loop.length; i++) {
          loop(i);
        }</script>|])

      it "renders css" $ do
        slimToEex (unpack [text|
        head
          css:
            .page {
              beautiful: you-bet;
            }
        |]) `shouldBe` ((init . unpack) [text|
        <head><style type="text/css">.page {
          beautiful: you-bet;
        }</style></head>|])

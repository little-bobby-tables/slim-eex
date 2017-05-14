{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module DriverSpec where
  import Driver

  import Test.Hspec

  import Data.Char (isSpace)
  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "slim to eex" $ do
      it "renders nested nodes" $ do
        slimToEex (unpack [text|
        head
          title
            nested
          meta
        body
          div
        |]) `shouldBe` (((filter (not . isSpace)) . unpack) [text|
        <head>
          <title>
            <nested></nested>
          </title>
          <meta/>
        </head>
        <body>
          <div></div>
        </body>
        |])

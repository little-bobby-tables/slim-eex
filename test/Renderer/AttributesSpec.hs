{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Renderer.AttributesSpec where
  import Driver

  import Test.Hspec

  import Data.Text (unpack)
  import NeatInterpolation (text)

  main :: IO ()
  main = hspec spec

  spec :: Spec
  spec = do
    describe "attribute values" $ do
      it "renders string attributes with escape sequences" $ do
        slimToEex (unpack [text|
        head
          meta name="description" content="What a \"ride\""
        |]) `shouldBe` (((filter (/= '\n')) . unpack) [text|
        <head>
        <meta name="description" content="What a \"ride\""/>
        </head>
        |])

      it "renders code attributes" $ do
        slimToEex (unpack [text|
        head
          meta name="description" content=page_description(@conn)
          meta name="author" content==unescaped(something)["k"]
        |]) `shouldBe` (((filter (/= '\n')) . unpack) [text|
        <head>
        <meta name="description" content="<%= page_description(@conn) %>"/>
        <meta name="author" content="<%= safe(unescaped(something)["k"]) %>"/>
        </head>
        |])

      it "renders boolean attributes" $ do
        slimToEex (unpack [text|
        video(loop muted playsinline poster=poster_url(v))
        |]) `shouldBe` (((filter (/= '\n')) . unpack) [text|
        <video loop muted playsinline poster="<%= poster_url(v) %>"></video>
        |])

    describe "shorthand attributes" $ do
      it "merges multiple class attributes" $ do
        slimToEex (unpack [text|
        div.so.many.classes class=dynamic("why not?") class==unescaped_too
        |]) `shouldBe` (((filter (/= '\n')) . unpack) [text|
        <div class="so many classes <%= dynamic("why not?") %> <%= safe(unescaped_too) %>"></div>
        |])

      it "merges multiple id attributes" $ do
        slimToEex (unpack [text|
        div#id1#id2 id=dynamic id==unescaped_too
        |]) `shouldBe` (((filter (/= '\n')) . unpack) [text|
        <div id="id1-id2-<%= dynamic %>-<%= safe(unescaped_too) %>"></div>
        |])

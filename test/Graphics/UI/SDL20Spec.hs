module Graphics.UI.SDL20Spec where

import Graphics.UI.SDL20

import Control.Exception
import Data.Version
import Data.List

import Test.Hspec


testlib :: LIBSDL -> IO (Either String ())
testlib lib =
  let maybeEx s = if (isPrefixOf "<sdlhs>" s) then Just s else Nothing
  in  tryJust (\(ErrorCall s) -> maybeEx s) (withlib lib (return ()))


main :: IO ()
main = hspec spec

spec :: Spec
spec = runIO (testlib SDL20) >>= \x -> case x of
  Left  xs -> it "cannot test this module" (pendingWith xs)
  Right () -> before (loadlib SDL20) $ after (freelib SDL20) $ do

  -----------------------------
  describe "sdlGetVersion" $ do
    it "Get the version of SDL (should be 2.0 or higher compatible)" $ do
      sdlGetVersion >>= (`shouldSatisfy` (>= Version [2,0,0] []))


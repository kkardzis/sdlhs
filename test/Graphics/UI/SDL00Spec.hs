module Graphics.UI.SDL00Spec where

--import Graphics.UI.SDL00

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "RTLD LIBSDL" $ do
    it "load/free libsdl" $ do
      pending


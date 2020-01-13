module BFInterpreterSpec (spec) where

import BFInterpreter
import Test.Hspec

testProgram = "+[-[<<[+[--->]-[<<<]]]>>>-]>-.---.>..>.<<<<-.<+.>>>>>.>.<<.<-."

spec :: Spec
spec = do
  describe "exec" $ do
    it "runs the Hello World program from Wikipedia" $
      exec testProgram `shouldBe` "hello world"

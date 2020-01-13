module ParserSpec (spec) where

import Control.Applicative
import Data.Either
import Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "char" $ do
      it "consumes a character when it is the head of the input string" $
        parse (char 'a' <* many item) "abc" `shouldBe` Right 'a'
      it "throws an error if the character isn't the head of the input string" $
        parse (char 'a' <* many item) "cba" `shouldSatisfy` isLeft

    describe "digit" $ do
      it "consumes any digit when it is the head of the input string" $
        parse (digit <* many item) "1bc" `shouldBe` Right '1'
      it "throws an error if the head of the input string isn't a digit" $
        parse (digit <* many item) "ab1" `shouldSatisfy` isLeft

    describe "integer" $ do
      it "consumes a positive integer when it is the head of the input string" $
        parse (integer <* many item) "12abc" `shouldBe` Right (12 :: Int)
      it "consumes a negative integer when it is the head of the input string" $
        parse (integer <* many item) "-12abc" `shouldBe` Right (-12 :: Int)
      it "throws an error if the beginning of the input string isn't an integer" $
        parse (digit <* many item) "abc12" `shouldSatisfy` isLeft

    describe "satisfy" $ do
      it "consumes a character given `const True`" $
        parse ((satisfy $ const True) <* many item) "abc" `shouldBe` Right 'a'
      it "doesn't consume a character given `const False`" $
        parse ((satisfy $ const False) <* many item) "abc" `shouldSatisfy` isLeft

    describe "sepBy" $ do
      it "modifies a parser to parse tokens separated by another parser" $
        parse (integer `sepBy` string ",") "1,2,3" `shouldBe` Right [1,2,3]
      it "consumes a single token followed by a delimiter" $
        parse (integer `sepBy` string ",") "1," `shouldBe` Right [1]
      it "consumes a single token with no delimiters" $
        parse (integer `sepBy` string ",") "1" `shouldBe` Right [1]
      it "doesn't consume anything if there is nothing to consume" $
        parse (integer `sepBy` string ",") "a,b,c" `shouldSatisfy` isLeft

    describe "spaces" $ do
      it "consumes spaces at the beginning of the input string" $
        parse (spaces <* many item) "    a  bc" `shouldBe` Right "    "
      it "doesn't consume letters" $
        parse (spaces <* many item) "a b c" `shouldBe` Right ""

    describe "string" $ do
      it "consumes a string matching the given string at the beginning of the input string" $
        parse (string "abc" <* many item) "abcdef" `shouldBe` Right "abc"
      it "doesn't consume a string that doesn't completely match the input string" $
        parse (string "acd" <* many item) "abcdef" `shouldSatisfy` isLeft

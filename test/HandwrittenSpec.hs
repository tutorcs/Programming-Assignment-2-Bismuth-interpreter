https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-- | Handwritten Bismuth programs.  This test suite compares the output of
-- `eval` with the images in handwritten-output/ directory.

module HandwrittenSpec where

import Data.Bifunctor (first, second)
import Parser
import Eval
import Rendering
import Image
import Color
import Test.Hspec
import System.IO
import qualified Codec.Picture as P
import qualified Data.Map.Strict as M

-- run the ungraded programs (they don't depend on any of the student
-- implementation)
ungradedPrograms = describe "ungraded programs" $ do
  mapM_ checkSuccess [ "x"
                     , "y"
                     , "r"
                     , "theta"
                     , "color"
                     , "color-expr"
                     , "color-expr2"
                     , "gray"
                     , "mask"
                     ]

-- run the small examples
handwritten = describe "handwritten" $ do
  describe "correct programs" $ mapM_ checkSuccess handwrittenTests
  describe "program that should fail" $ mapM_ checkFailure testsThatShouldFail

-- large examples
largeExamples = describe "example" $ do
  describe "large example programs" $ mapM_ checkSuccess largeExamplePrograms

handwrittenTests = [ "binop"
                   , "circle"
                   , "flip"
                   , "if"
                   , "juxtaposeH"
                   , "juxtaposeV"
                   , "let"
                   , "overlay"
                   , "rgb"
                   , "rotate"
                   , "scale2"
                   , "scale"
                   , "shadow"
                   , "swirl-arith"
                   , "translate"
                   , "unop2"
                   , "unop"
                   ]

testsThatShouldFail = [ "binop"
                      , "if2"
                      , "if"
                      , "juxtaposeH"
                      , "juxtaposeV"
                      , "let"
                      , "overlay"
                      , "var"
                      ]

largeExamplePrograms = [ "star"
                     , "heart"
                     , "lambda-quilt"
                     , "lambda-color2"
                     , "nautilus"
                     ]

extraCreditExamples = [ "four-at-once"
                      , "lambda-color"
                      , "usa"
                      ]

checkSuccess :: String -> SpecWith (Arg Expectation)
checkSuccess name = it (name ++ " should be rendered correctly") $ do
  imageFromFile <- P.readPng imageFile
  program <- parseProgram' biFile <$> readFile' biFile
  case (first (ParseError . show) program) >>= run of
    Left (ParseError message) -> expectationFailure $ "failed to parse " ++ biFile ++ ": " ++ message
    Left (RuntimeError message) -> expectationFailure $ "failed to evaluate " ++ biFile ++ ": " ++ message
    Right image -> (P.ImageRGBA8 image == get imageFromFile) `shouldBe` True
  where
    get (Left _) = undefined
    get (Right r) = r
    biFile = "handwritten/" ++ name ++ ".bi"
    imageFile = "handwritten-output/" ++ name ++ ".png"

checkFailure name = it (name ++ " should give a runtime error") $ do
  program <- parseProgram' biFile <$> readFile' biFile
  case (first (ParseError . show) program) >>= run of
    Left (ParseError message) -> expectationFailure $ "failed to parse " ++ biFile ++ ": " ++ message
    Left (RuntimeError _) -> return ()
    Right _ -> expectationFailure $ biFile ++ " should have failed to evaluate."
  where
    biFile = "handwritten-failure/" ++ name ++ ".bi"

extraCreditTests = describe "extra-credit" $ do
  describe "small juxtapose tests that should pass" $ do
    mapM_ checkSuccess ["rgb"]
  describe "small juxtapose tests that fail" $ do
    mapM_ checkFailure ["juxtapose", "juxtapose2"]
  describe "small replicate tests that should pass" $ do
    mapM_ checkSuccess ["vstripe"]
  describe "large extra-credit tests" $ do
    mapM_ checkSuccess extraCreditExamples

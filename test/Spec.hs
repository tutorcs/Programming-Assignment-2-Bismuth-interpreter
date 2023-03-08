https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
import Test.Hspec
import PointSpec
import ColorSpec
import ImageSpec

import HandwrittenSpec

main :: IO ()
main = hspec $ do
  colorSpec
  pointSpec
  imageSpec
  examples
  ungradedPrograms
  handwritten
  largeExamples
  extraCreditTests

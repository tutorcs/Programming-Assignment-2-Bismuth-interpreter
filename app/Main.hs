https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
module Main (main) where

import Color
import Parser
import Image
import Example
import Rendering
import Eval
import qualified Codec.Picture as P
import System.Environment (getArgs)
import Data.Bifunctor (first, second)
import System.IO

renderWithDims width height = second $ \im -> render im width height

renderExample :: (PixelValue c) => (a, Image c) -> (a, P.Image P.PixelRGBA8)
renderExample = renderWithDims 512 512

main :: IO ()
main = getArgs >>= actualMain

actualMain [biFile, outputFile] = do
  program <- parseProgram' biFile <$> readFile' biFile
  case program of
    Left _ -> return ()
    Right p -> print p
  case (first convertError program) >>= run of
    Left (ParseError message) -> putStrLn $ "Parse error: " ++ message
    Left (RuntimeError message) -> putStrLn $ "Runtime error: " ++ message
    Right im -> P.writePng outputFile im
actualMain _ =
  putStrLn "usage: stack run -- bismuth-program output-file"

convertError e = ParseError $ show e

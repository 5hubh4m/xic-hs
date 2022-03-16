module Main where

import           Text.Megaparsec
import           Xi.Parse
import qualified Data.Text.IO    as T

main :: IO ()
main = (parseTest program) =<< T.getContents

module Main (main) where

import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Revna.Location
import Revna.Syntax

main :: IO ()
main = do
  let filepath = "samples/Main.ra"
  contents <- BS.readFile filepath
  case runLexer filepath contents of
    Right tokens -> forM_ tokens (print . unlocated)
    Left err -> print err

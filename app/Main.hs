module Main (main) where

import Data.ByteString qualified as BS
import Revna.Diagnostic
import Revna.Syntax
import Revna.Typer.Env
import Revna.Typer.Infer

main :: IO ()
main = do
  let filepath = "samples/Main.ra"
  contents <- BS.readFile filepath

  case runParseModule filepath contents of
    Right tree ->
      let module' = translModule tree
       in case runTc (TcM (checkModule module')) of
            Right _ -> print ()
            Left err -> putStrLn (renderDiagnostic err contents)
    Left err -> putStrLn (renderDiagnostic err contents)

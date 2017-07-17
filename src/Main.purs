module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (intercalate)
import Node.FS.Sync (writeTextFile)
import Node.Encoding
import Node.ChildProcess

basicPackages = [
    "Prelude"
  , "Control.Monad.Eff.Console (CONSOLE, log)"
  ]

type PackageName = String
type PackageNameWithImport = String
type ExprStr = String
type PsFileContent = String

appendImport :: PackageName -> PackageNameWithImport
appendImport = append "import "

psFile :: Array PackageName -> ExprStr -> PsFileContent
psFile _ _ = ?psFile
psFile pkgs exprStr = (intercalate "\n" $ map appendImport pkgs) <> "\n\n" <> exprStr

writePsFile exprStr = writeTextFile UTF8 "./test.purs" $ psFile basicPackages exprStr

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

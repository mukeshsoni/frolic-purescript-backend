module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (intercalate)
import Node.FS.Sync (writeTextFile)
import Node.Encoding (Encoding(..))
import Node.ChildProcess (spawn, defaultSpawnOptions, stdout, stderr, onExit, Exit(..))
import Node.Stream (onDataString)

moduleDeclaration = "module Main where"

basicPackages = [
    "Prelude"
  , "Control.Monad.Eff.Console (CONSOLE, log)"
  ]

psSrcPath = "temp"

type PackageName = String
type PackageNameWithImport = String
type ExprStr = String
type PsFileContent = String

appendImport :: PackageName -> PackageNameWithImport
appendImport = append "import "

psFile :: Array PackageName -> ExprStr -> PsFileContent
psFile pkgs exprStr = moduleDeclaration <> "\n\n" <> (intercalate "\n" $ map appendImport pkgs) <> "\n\n" <> exprStr

writePsFile exprStr = writeTextFile UTF8 (psSrcPath <> "/Main.purs") $ psFile basicPackages exprStr

buildPsFile exprStr = do
  cp <- spawn "pulp" ["build", "--src-path", "temp", "--to", "temp/output.js"] defaultSpawnOptions
  let stout = stdout cp
      sterr = stderr cp
  onDataString stout UTF8 \s ->
    log (s <> " yipppeee!!")

  onDataString sterr UTF8 \s ->
    log ("Erorrrrrr! " <> s)

  onExit cp \e -> case e of
    Normally 0 -> log (show e)
    Normally code -> log (show code)
    BySignal _ -> pure unit

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

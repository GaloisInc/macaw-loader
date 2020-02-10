{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Monad (when, forM_)
import qualified Data.ByteString as B
import           System.Directory (listDirectory)
import           System.Exit (exitFailure, ExitCode(..))
import qualified System.Process as Proc

import qualified Data.ElfEdit as E
import           Data.Macaw.BinaryLoader (loadBinary, LoadedBinary)
import           Data.Macaw.BinaryLoader.X86 () -- for BinaryLoader instance
import qualified Data.Macaw.Memory.LoadCommon as MM
import           Data.Macaw.X86 (X86_64)

-- Tasty
import           Test.Tasty
import           Test.Tasty.HUnit

-- | Compile a C file with gcc, returning the exit code
compile :: FilePath -> IO (ExitCode, String, String)
compile !file =
  Proc.readProcessWithExitCode "gcc" ["-g", "-O0", "-o", file ++ ".bin", file] ""

main :: IO ()
main = do
  let assertSuccess msg file (exitCode, stdout, stderr) = do
        when (exitCode /= ExitSuccess) $ do
          putStrLn $ msg ++ " " ++ file
          putStrLn stdout
          putStrLn stderr
          exitFailure

  let hasExtension ext file = reverse ext == take (length ext) (reverse file)
  let cdir = "test/c/"
  cfiles <- filter (hasExtension ".c") . map (cdir ++) <$> listDirectory cdir
  forM_ cfiles $ \cfile ->
    assertSuccess "compile" cfile =<< compile cfile
  defaultMain $ tests (map (++".bin") cfiles)

tests :: [FilePath] -> TestTree
tests binaries = testGroup "Tests" $ concat $
  [ flip map binaries $ \binary -> testCase binary $ withElf binary $ \elf -> do
      let loadOpts = MM.defaultLoadOptions { MM.loadOffset = Just 0 }
      _loadedBinary
        <- loadBinary loadOpts elf :: IO (LoadedBinary X86_64 (E.Elf 64))
      pure ()
  ]

-- from the renovate test suite
withElf :: FilePath -> (E.Elf 64 -> IO a) -> IO a
withElf fp k = do
  bytes <- B.readFile fp
  case E.parseElf bytes of
    E.ElfHeaderError off msg ->
      error ("Error parsing ELF header at offset " ++ show off ++ ": " ++ msg)
    E.Elf32Res [] _e32 -> error "ELF32 is unsupported in the test suite"
    E.Elf64Res [] e64 -> k e64
    E.Elf32Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)
    E.Elf64Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)

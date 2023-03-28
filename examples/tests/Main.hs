
module Main where

--------------------------------------------------------------------------------

import System.Environment

import qualified MOOC.Main      as Lab
import qualified Simple.Main    as Simple
import qualified CircomLib.Main as CircomLib

--------------------------------------------------------------------------------

help = do
  putStrLn "usage:"
  putStrLn ""
  putStrLn "$ r1cs-solver-examples <testsuite>"
  putStrLn ""
  putStrLn "where <testsuite> is one of the following:"
  putStrLn ""
  putStrLn " - examples"
  putStrLn " - zkp-mooc-lab"
  putStrLn " - mooc-floatadd"
  putStrLn " - circomlib"
  putStrLn ""

--------------------------------------------------------------------------------

main = do
  args <- getArgs 
  case args of

    [what] -> case what of

      "ex"             -> Simple.testSimple
      "examples"       -> Simple.testSimple
      "simple"         -> Simple.testSimple

      "zkp-mooc-lab"   -> Lab.testLab
      "mooc"           -> Lab.testLab
      "lab"            -> Lab.testLab

      "floatadd"       -> Lab.testFloatAdd
      "mooc-floatadd"  -> Lab.testFloatAdd

      "circom"         -> CircomLib.testCircomLib
      "circomlib"      -> CircomLib.testCircomLib

      _ -> do
        putStrLn $ "unknown testsuite `" ++ what ++ "`"
        help
 
    _ -> help

--------------------------------------------------------------------------------


module MOOC.Common
  ( module Data.Map
  , module Control.Concurrent.MVar
  , module System.FilePath
  , module R1CS
  , Bit(..)
  , circuitSourceDir , labCircomFile
--  , theCircuitFiles
--  , compileLabCircuit
--  , runExample_
--  , printTheConstraints
  , Verbosity (..)
  ) 
  where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Concurrent.MVar

import Data.Map (Map)  
import qualified Data.Map  as Map

import System.FilePath
import System.IO.Unsafe

import R1CS
import R1CS.Misc

--------------------------------------------------------------------------------

circuitSourceDir :: FilePath 
circuitSourceDir = "examples/circuits/zkp-mooc-lab/"  

labCircomFile :: FilePath
labCircomFile = circuitSourceDir </> "float_add_unsolved.circom"

--------------------------------------------------------------------------------
{-

-- | This a global variable storing the circuit files. It is only here as
-- a convenience when experimenting in GHCi, so that we don't have to
-- recompile the same things over-and-over
--
{-# NOINLINE theCircuitFiles #-}
theCircuitFiles :: MVar CircuitFiles
theCircuitFiles = unsafePerformIO newEmptyMVar

{-# NOINLINE theConstraints #-}
theConstraints :: MVar (SymConstraints Field)
theConstraints = unsafePerformIO newEmptyMVar

--------------------------------------------------------------------------------

-- | Compile a single circuit, and caches the resulting files
compileLabCircuit :: Verbosity -> FilePath -> Maybe MainComponent -> IO CircuitFiles
compileLabCircuit verbosity circomfile mainComp = do

  -- compile with circom
  circuitFiles <- compileCircomCircuit verbosity (circuitSourceDir </> circomfile) mainComp
  replaceMVar theCircuitFiles circuitFiles

  -- parse the results (just to cache it)
  constraints <- loadCircuit Silent circuitFiles
  replaceMVar theConstraints constraints
  
  -- return the files
  return circuitFiles

--------------------------------------------------------------------------------

reloadCircuit :: Verbosity -> IO (SymConstraints Field)
reloadCircuit verbosity = do
  circuitFiles <- readMVar theCircuitFiles
  constraints  <- loadCircuit verbosity circuitFiles
  replaceMVar theConstraints constraints
  return constraints

printTheConstraints :: IO ()
printTheConstraints = do
  constr <- readMVar theConstraints
  printSymConstraints prettyField constr

--------------------------------------------------------------------------------

-- | Execute the cached circuit on the give input. Useful for experimenting in GHCi
runExample_ :: Inputs Name Integer -> IO ()
runExample_ inputs = do

  circuitFiles <- readMVar theCircuitFiles

  sols <- runExample Verbose circuitFiles inputs

  let nsols = length sols

  forM_ (zip [1..] sols) $ \(i,sol) -> do
    putStrLn "_______________________"
    putStrLn $ "solution #" ++ show i ++ " (out of " ++ show nsols ++ ")"
    printSolution sol

-}
--------------------------------------------------------------------------------

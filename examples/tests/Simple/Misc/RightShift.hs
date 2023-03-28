
module Simple.Misc.RightShift where

--------------------------------------------------------------------------------

import Data.Bits
import Control.Monad

import qualified Data.Map as Map

import Simple.Common

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "rightshift_unsound.circom"

shiftBy :: Int
shiftBy = 3

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "RightShiftUnsound"
  , _templateParams = [shiftBy]
  , _publicInputs   = ["inp"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = Int
type Output   = Int

semantics :: TestCase -> Expected Output
semantics n = Expecting $ shiftR n shiftBy

testCases :: [TestCase]
testCases = [0..25]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs x = Inputs $ toMapping "inp" x

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "out" y

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec circomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------
-- test the honest prover, too

testHonestProver :: Verbosity -> IO ()
testHonestProver verbosity = do
  putStrLn $ "now, let's test the honest prover:"
  circuitfiles <- compileCircomCircuit verbosity circomFile (Just mainComponent)
  forM_ testCases $ \inp -> do
    let hsinputs = HsInputs $ Map.singleton "inp" (HsInput inp)
    witnessfiles <- computeWitness verbosity circuitfiles hsinputs
    witness <- loadWitness verbosity circuitfiles witnessfiles
    -- print witness
    let Witness table = witness
    let expected = semantics inp
    let out = (Map.!) (fromMapping table) "main.out"
    putStrLn $ "input = " ++ show inp ++ " | output = " ++ show out ++ " | expected = " ++ show expected
  putStrLn $ "and it looks just fine."

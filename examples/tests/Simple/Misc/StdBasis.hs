
module Simple.Misc.StdBasis where

--------------------------------------------------------------------------------

import Simple.Common

--------------------------------------------------------------------------------
-- global parameters

n :: Int
n = 13

circomFile :: FilePath
circomFile = circuitSourceDir </> "standard_basis.circom"

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "StdBasisVector"
  , _templateParams = [n]
  , _publicInputs   = ["idx"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = Int
type Output   = [Bit]

basisVector :: Int -> [Bit]
basisVector k = replicate k Zero ++ [One] ++ replicate (n-k-1) Zero

semantics :: TestCase -> Expected Output
semantics idx
  | idx >= 0 && idx < n = Expecting (basisVector idx)
  | otherwise           = ShouldFail

testCases :: [TestCase]
testCases = [-2..n+2]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs j = Inputs $ toMapping "idx" j

outputs :: Output -> Outputs Name Integer
outputs bits = Outputs $ toMapping "vec" bits

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec circomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------



-- | MSNZB is short for \"most significant non-zero bit\"

module MOOC.Lab.MSNZB where

--------------------------------------------------------------------------------

import Data.Bits
import Data.List

import MOOC.Common

--------------------------------------------------------------------------------
-- global parameters

nbits :: Int
nbits = 6

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "MSNZB"
  , _templateParams = [nbits]
  , _publicInputs   = ["in","skip_checks"]
  }

type TestCase = (Int,Bool)
type Output   = [Bit]

--------------------------------------------------------------------------------
-- test cases and expected semantics

msnzb :: Int -> Int
msnzb 0 = -1
msnzb 1 =  0
msnzb k = 1 + msnzb (shiftR k 1)

onehot :: Int -> [Bit]
onehot k 
  | k >= 0 && k < nbits  = replicate k Zero ++ [One] ++ replicate (nbits - k - 1) Zero
  | otherwise            = replicate nbits Zero

semantics :: TestCase -> Expected Output
semantics (x,skip_checks) 
  | skip_checks           = DontCare
  | x > 0 && x < 2^nbits  = Expecting $ onehot $ msnzb x
  | otherwise             = ShouldFail

testCases :: [TestCase]
testCases 
  =  [ (x,False) | x <- [-2..(2^nbits)+2] ]
  ++ [ (x,True ) | x <- [-2..(2^nbits)+2] ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs (x,skip_checks) = Inputs 
  $  toMapping "in"          x
  <> toMapping "skip_checks" skip_checks

-- expected outputs
outputs :: Output -> Outputs Name Integer
outputs bits = Outputs $ toMapping "one_hot" bits

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec labCircomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------

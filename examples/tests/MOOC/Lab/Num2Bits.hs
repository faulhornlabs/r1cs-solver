
module MOOC.Lab.Num2Bits where

--------------------------------------------------------------------------------

import Data.Bits
import MOOC.Common

--------------------------------------------------------------------------------
-- global parameters

nbits :: Int
nbits = 7

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "Num2Bits"
  , _templateParams = [nbits]
  , _publicInputs   = ["in"]
  }

type TestCase = (Int)
type Output   = [Bit]

--------------------------------------------------------------------------------
-- test cases and expected semantics

toBits :: Int -> [Bit]
toBits n = [ if (shiftR n k .&. 1)> 0 then One else Zero | k <-[0..nbits-1] ]

semantics :: TestCase -> Expected Output
semantics n 
  | (n >= 0) && (n < 2^nbits)  = Expecting (toBits n)
  | otherwise                  = ShouldFail

testCases :: [TestCase]
testCases = [-5..130] 

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs x = Inputs $ toMapping "in" x

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "bits" y

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec labCircomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------

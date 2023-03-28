
module MOOC.Lab.RightShift where

--------------------------------------------------------------------------------

import Data.Bits
import MOOC.Common

--------------------------------------------------------------------------------
-- global parameters

bound, shiftby :: Int
bound   = 8
shiftby = 3

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "RightShift"
  , _templateParams = [bound,shiftby]
  , _publicInputs   = ["x"]
  }

type TestCase = Int
type Output   = Int

--------------------------------------------------------------------------------
-- test cases and expected semantics

semantics :: TestCase -> Expected Output
semantics n  
  | (n >= 0) && (n < 2^bound)  = Expecting (shiftR n shiftby)
  | otherwise                  = ShouldFail

testCases :: [TestCase]
testCases = [-5..260] 

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs x = Inputs $ toMapping "x" x

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "y" y

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec labCircomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------

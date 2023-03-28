
module MOOC.Lab.CheckBitLength where

--------------------------------------------------------------------------------

import MOOC.Common

--------------------------------------------------------------------------------
-- global parameters

nbits :: Int
nbits = 7

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "CheckBitLength"
  , _templateParams = [nbits]
  , _publicInputs   = ["in"]
  }

type TestCase = Int
type Output   = Bool

--------------------------------------------------------------------------------
-- test cases and expected semantics

-- | NOTE: this is a very strange semantics, because we *allow* false negatives!
-- However, apparently that shouldn't case a problem in bigger context, because 
-- in that case the proof will fail later (?)
semantics :: TestCase -> Expected Output
semantics n = if (n >= 0) && (n < 2^nbits)
  then OneOf [True,False]
  else Expecting False

testCases :: [TestCase]
testCases = [-5..260] 

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs x = Inputs $ toMapping "in" x

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "out" y

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec labCircomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------


module MOOC.Lab.LessThan where

--------------------------------------------------------------------------------

import MOOC.Common

--------------------------------------------------------------------------------
-- global parameters

nbits :: Int
nbits = 5

bound :: Int
bound = 2^nbits

testRange :: [Int]
testRange = [-2..bound+3]

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "LessThan"
  , _templateParams = [nbits]
  , _publicInputs   = ["in"]
  }

type TestCase = (Int,Int)
type Output   = Bool

--------------------------------------------------------------------------------
-- test cases and expected semantics

semantics :: TestCase -> Expected Output
semantics (a,b) 
  | a >= 0 && a < bound && 
    b >= 0 && b < bound       = Expecting (a < b) 
  | otherwise                 = ShouldFail -- ontCare

testCases :: [TestCase]
testCases = [ (a,b) | a<-testRange, b<-testRange ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs ab = Inputs $ toMapping "in" ab

-- expected outputs
outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "out" y

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec labCircomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------

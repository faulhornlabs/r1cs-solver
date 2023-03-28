
module MOOC.Lab.LeftShift where

--------------------------------------------------------------------------------

import MOOC.Common

--------------------------------------------------------------------------------
-- global parameters

shiftbound :: Int
shiftbound = 8

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "LeftShift"
  , _templateParams = [shiftbound]
  , _publicInputs   = ["x","shift","skip_checks"]
  }

type TestCase = (Int,Int,Bool)
type Output   = Int

--------------------------------------------------------------------------------
-- test cases and expected semantics

semantics :: TestCase -> Expected Output
semantics (x,shift,skip_checks) 
  | skip_checks                        = DontCare
  | shift >=0 && (shift < shiftbound)  = Expecting (2^shift * x)
  | otherwise                          = ShouldFail 

testCases :: [TestCase]
testCases 
  =  [ (  0,s,False) | s <- [-2..shiftbound+2] ]
  ++ [ (  1,s,False) | s <- [-2..shiftbound+2] ]
  ++ [ (123,s,False) | s <- [-2..shiftbound+2] ]
  ++ [ ( 57,s,False) | s <- [-2..shiftbound+2] ]
  ++ [ ( 22,s,True ) | s <- [-2..shiftbound+2] ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs (x,shift,skip_checks) = Inputs 
  $  toMapping "x"           x
  <> toMapping "shift"       shift
  <> toMapping "skip_checks" skip_checks

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "y" y

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec labCircomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------

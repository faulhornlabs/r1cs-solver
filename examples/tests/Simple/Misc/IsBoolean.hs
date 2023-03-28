
module Simple.Misc.IsBoolean where

--------------------------------------------------------------------------------

import Simple.Common

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "is_boolean.circom"

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "IsBoolean"
  , _templateParams = []
  , _publicInputs   = ["in"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = Int
type Output   = Bool

semantics :: TestCase -> Expected Output
semantics n = Expecting $ (n == 0) || (n==1)

testCases :: [TestCase]
testCases = [-5..35] 

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs x = Inputs $ toMapping "in" x

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "out" y

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec circomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------


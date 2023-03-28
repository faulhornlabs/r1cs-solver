
module Simple.Misc.SumOfCubes where

--------------------------------------------------------------------------------

import Simple.Common

--------------------------------------------------------------------------------
-- global parameters

vecSize :: Int
vecSize = 7

circomFile :: FilePath
circomFile = circuitSourceDir </> "sum_of_cubes.circom"

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "SumOfCubes"
  , _templateParams = [vecSize]
  , _publicInputs   = ["inp"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = [Integer]
type Output   =  Integer

semantics :: TestCase -> Expected Output
semantics vec = Expecting $ sum [ x^3 | x <- vec ]

testCases :: [TestCase]
testCases = 
  [ [1..7]
  , [11..17]
  , [21..27]
  , [5,3,10,15,23,17,6]
  ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs xs = Inputs $ toMapping "inp" xs

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "out" y

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec circomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------


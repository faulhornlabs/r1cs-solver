
module CircomLib.Lib.Switcher where

--------------------------------------------------------------------------------

import CircomLib.Common

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "switcher.circom"

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "Switcher"
  , _templateParams = []
  , _publicInputs   = ["sel","L","R"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = (Bool,(Integer,Integer))
type Output   =       (Integer,Integer)

semantics :: TestCase -> Expected Output
semantics (b,(x,y)) = Expecting $ if b then (y,x) else (x,y)

testCases :: [TestCase]
testCases = list where
  list = cases True ++ cases False 
  cases b = 
    [ (b , (100,20 )) 
    , (b , (13 ,256)) 
    , (b , (-5 ,137)) 
    , (b , (13, 13 )) 
    ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs (sel,(x,y)) = Inputs $  toMapping "sel" sel
                            <> toMapping "L"   x
                            <> toMapping "R"   y

outputs :: Output -> Outputs Name Integer
outputs (x,y) = Outputs $  toMapping "outL" x
                        <> toMapping "outR" y

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec circomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------


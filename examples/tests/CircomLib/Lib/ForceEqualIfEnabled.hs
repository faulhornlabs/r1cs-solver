
module CircomLib.Lib.ForceEqualIfEnabled where

--------------------------------------------------------------------------------

import CircomLib.Common

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "cmp.circom"

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "ForceEqualIfEnabled"
  , _templateParams = []
  , _publicInputs   = ["enabled","in"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = (Bool,(Integer,Integer))
type Output   = ()

semantics :: TestCase -> Expected Output
semantics (b,(x,y)) = 
  if b
    then if (x==y) 
      then Expecting ()
      else ShouldFail
    else DontCare

testCases :: [TestCase]
testCases = list where
  list = cases True ++ cases False 
  cases b = 
    [ (b , (100,20 )) 
    , (b , (13 ,256)) 
    , (b , (-5 ,137)) 
    , (b , (13, 13 )) 
    , (b , (-7, -7 )) 
    ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs (en,(x,y)) = Inputs $  toMapping "enabled" en
                           <> toMapping "in"      (x,y)

outputs :: Output -> Outputs Name Integer
outputs () = Outputs $ emptyMapping

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec circomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------


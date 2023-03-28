
module CircomLib.Lib.Comparators where

--------------------------------------------------------------------------------

import CircomLib.Common

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "cmp.circom"

-- global parameter = which one we test
data GP 
  = IsEqual
  | LessThan
  | LessEqThan
  | GreaterThan
  | GreaterEqThan
  deriving (Eq,Ord,Show,Enum,Bounded)

mainComponent :: GP -> MainComponent
mainComponent gp = MainComponent 
  { _templateName   = show gp
  , _templateParams = if (gp == IsEqual) then [] else [12]
  , _publicInputs   = ["in"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = (Int,Int)
type Output   = Bool

semantics :: GP -> TestCase -> Expected Output
semantics which (x,y) = Expecting $ case which of
  IsEqual       -> x == y
  LessThan      -> x <  y
  LessEqThan    -> x <= y
  GreaterThan   -> x >  y
  GreaterEqThan -> x >= y

testRange = [-3..17]

testCases :: GP -> [TestCase]
testCases _ = [ (x,y) | x <- testRange, y <- testRange ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs (x,y) = Inputs $ toMapping "in" (x,y)

outputs :: Output -> Outputs Name Integer
outputs z = Outputs $ toMapping "out" z

--------------------------------------------------------------------------------

spec :: GP -> TestSpec TestCase Output
spec which = TestSpec circomFile (mainComponent which) inputs outputs (semantics which) (testCases which)

specs :: [ (GP, TestSpec TestCase Output) ]
specs = [ (which, spec which) | which<-[minBound..maxBound] ]

--------------------------------------------------------------------------------


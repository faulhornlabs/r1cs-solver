
module CircomLib.Lib.MultiAND where

--------------------------------------------------------------------------------

import CircomLib.Common

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "gates.circom"

-- global parameter = size of the input vector
type GP = Int

mainComponent :: GP -> MainComponent
mainComponent n = MainComponent 
  { _templateName   = "MultiAND"
  , _templateParams = [n]
  , _publicInputs   = ["in"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = [Bool]
type Output   = Bool

semantics :: TestCase -> Expected Output
semantics bs = Expecting $ and bs

onehot :: Int -> Int -> (a,a) -> [a]
onehot n k (x,y) = replicate k x ++ [y] ++ replicate (n-k-1) x

switchAt :: Int -> Int -> (a,a) -> [a]
switchAt n k (x,y) = replicate k x ++ replicate (n-k) y

testCases :: GP -> [TestCase]
testCases n = 
  [ replicate n True
  , replicate n False
  ] ++
  [ onehot   n i (True,False) | i<-[0..n-1] ] ++
  [ onehot   n i (False,True) | i<-[0..n-1] ] ++
  [ switchAt n i (True,False) | i<-[0..n-1] ] ++
  [ switchAt n i (False,True) | i<-[0..n-1] ] 

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs xs = Inputs $ toMapping "in" xs

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "out" y

--------------------------------------------------------------------------------

spec :: GP -> TestSpec TestCase Output
spec n = TestSpec circomFile (mainComponent n) inputs outputs semantics (testCases n)

specs :: [ (GP, TestSpec TestCase Output) ]
specs = [ (n, spec n) | n<-[1..7] ]

--------------------------------------------------------------------------------


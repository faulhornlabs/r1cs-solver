
module Simple.Misc.Fibonacci where

--------------------------------------------------------------------------------

import Simple.Common

--------------------------------------------------------------------------------
-- global parameters

circomFile :: FilePath
circomFile = circuitSourceDir </> "fibonacci.circom"

-- | we want the @n@-th element of the sequence
type GP = Int

mainComponent :: GP -> MainComponent
mainComponent n = MainComponent 
  { _templateName   = "Fibonacci"
  , _templateParams = [n]
  , _publicInputs   = ["in"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type TestCase = (Int,Int)
type Output   = Int

fib :: Int -> Int -> [Int]
fib a b = a : fib b (a+b)

semantics :: GP -> TestCase -> Expected Output
semantics n (a,b) = Expecting $ fib a b !! n

testCases :: [TestCase]
testCases = [ (a,b) | a<-[-2..3] , b<-[-2..3] ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs ab = Inputs $ toMapping "in" ab

outputs :: Output -> Outputs Name Integer
outputs y = Outputs $ toMapping "out" y

--------------------------------------------------------------------------------

spec :: GP -> TestSpec TestCase Output
spec n = TestSpec circomFile (mainComponent n) inputs outputs (semantics n) testCases

specs :: [ ( GP, TestSpec TestCase Output) ]
specs = [ (n, spec n) | n <- [0,1,2,3,5,7,12] ]

--------------------------------------------------------------------------------


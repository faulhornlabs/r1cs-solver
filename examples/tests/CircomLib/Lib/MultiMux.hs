
module CircomLib.Lib.MultiMux where

--------------------------------------------------------------------------------

import Data.Bits
import System.Random

import R1CS.Misc 
import CircomLib.Common

--------------------------------------------------------------------------------
-- global parameters

-- | @(k,m)@ where we are doing @mux<K>@ and @N=size of output vector@
type GP = (Int,Int)

circomFile :: GP -> FilePath
circomFile (k,_) = circuitSourceDir </> ("mux" ++ show k ++ ".circom")

mainComponent :: GP -> MainComponent
mainComponent (k,n) = MainComponent 
  { _templateName   = "MultiMux" ++ show k
  , _templateParams = [n]
  , _publicInputs   = ["c","s"]
  }

--------------------------------------------------------------------------------
-- test cases and expected semantics

type Vector   = [Int]
type Selector = [Bit]

type TestCase = ([Vector] , Selector)
type Output   = Vector

decodeSelector :: [Bit] -> Int
decodeSelector bits = go $ map bitToInt bits where
  go []     = 0
  go (a:as) = a + shiftL (go as) 1

-- | First input is the length of the resulting vector, second the number we are encoding
encodeSelector :: Int -> Int -> [Bit]
encodeSelector k a = take k (encodeSelector_ a ++ repeat Zero)

encodeSelector_ :: Int -> [Bit]
encodeSelector_ a = go a where
  go 0 = []
  go x = (if x .&. 1 == 0 then Zero else One) : go (shiftR x 1)

semantics :: TestCase -> Expected Output
semantics (vecs,sel) = Expecting $ (vecs !! decodeSelector sel)

testCases :: GP -> [TestCase]
testCases (k,n) = 
  [ (vecs , encodeSelector k sel) 
  | sel<-[0..pow2k-1] 
  , let vecs = map (take n) $ take pow2k $ drop sel $ testVectors
  ]
  where
    pow2k = 2^k

-- doubly infinite random vectors
testVectors :: [[Int]]
testVectors = map mkTestVector [0..]

-- an infinite test vector
mkTestVector :: Int -> [Int]
mkTestVector idx = randomRs (-100,1100) gen where
  gen = mkStdGen (137 + 101*idx)

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs (xxs,sel) = Inputs $  toMapping "c" xxs
                          <> toMapping "s" (toSingletonOrList sel)

outputs :: Output -> Outputs Name Integer
outputs ys = Outputs $ toMapping "out" ys

--------------------------------------------------------------------------------

spec :: GP -> TestSpec TestCase Output
spec gp = TestSpec (circomFile gp) (mainComponent gp) inputs outputs semantics (testCases gp)

specs :: [ (GP, TestSpec TestCase Output) ]
specs = [ (gp, spec gp) | k<-[1..4] , n<-[1,3,7] , let gp = (k,n) ]

--------------------------------------------------------------------------------


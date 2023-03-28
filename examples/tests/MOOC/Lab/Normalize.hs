
module MOOC.Lab.Normalize where

--------------------------------------------------------------------------------

import Data.Bits
import MOOC.Common

--------------------------------------------------------------------------------
-- global parameters

exponentBits, smallP, bigP :: Int
exponentBits = 3
smallP       = 4
bigP         = 6

expoRange :: [Int]
expoRange = [0..2^exponentBits-1]

mantissaRange :: [Int]
mantissaRange = [0..2^(bigP+1)+2]

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "Normalize"
  , _templateParams = [exponentBits, smallP, bigP]
  , _publicInputs   = ["e","m","skip_checks"]
  }

type TestCase = (Int,Int,Bool)
type Output   = (Int,Int)

--------------------------------------------------------------------------------
-- test cases and expected semantics

-- | Most Significant Non-Zero Bit
msnzb :: Int -> Int
msnzb 0 = -1
msnzb 1 =  0
msnzb k =  1 + msnzb (shiftR k 1)

-- | Recall the specification:
--
--  "Let ell be the MSNZB of m. Recall that m is a P+1-bit number with precision p.
--  We want to make the mantissa normalized, i.e., bring it to the 
--  range [2^P, 2^(P+1)), by shifting it left by P-ell bits.
--  Consequently, we need to decrement the exponent by P-ell.
--  At the same time, we are also increasing precision of mantissa from p to P, 
--  so we also need to increment the exponent by P-p.
--  Overall, this means adding (P-p)-(P-ell) = ell-p to the exponent."
--
-- and the Python reference implementation:
--
-- > ell = msnzb(m, P+1)
-- > m <<= (P - ell)
-- > e = e + ell - p
-- > return (e, m)
--
semantics :: TestCase -> Expected Output
semantics (e,m,skip_checks)

  | skip_checks                   = DontCare

  | (m > 0) && (m < 2^(bigP+1))   = let ell = msnzb m 
                                        m'  = shiftL m (bigP - ell) 
                                        e'  = e + ell - smallP 
                                    in  Expecting (e',m')

  | otherwise                     = ShouldFail

testCases :: [TestCase]
testCases  
  =  [ (e,m,False) | e<-expoRange, m<-mantissaRange      ]
  ++ [ (e,m,True ) | e<-[0,1,5]  , m<-[0,1,10,20,10000]  ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs (e,m,skip_checks) =  Inputs 
                         $  toMapping "e" e
                         <> toMapping "m" m
                         <> toMapping "skip_checks" skip_checks

outputs :: Output -> Outputs Name Integer
outputs (e,m) =  Outputs 
              $  toMapping "e_out" e 
              <> toMapping "m_out" m

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec labCircomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------

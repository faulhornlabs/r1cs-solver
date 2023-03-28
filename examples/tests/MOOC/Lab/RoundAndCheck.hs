
module MOOC.Lab.RoundAndCheck where

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
mantissaRange = [0..2^(bigP+1)-1]

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "RoundAndCheck"
  , _templateParams = [exponentBits, smallP, bigP]
  , _publicInputs   = ["e","m"]
  }

type TestCase = (Int,Int)
type Output   = (Int,Int)

--------------------------------------------------------------------------------
-- test cases and expected semantics

-- | Recall the specification:
--
-- "Rounds the input floating-point number and checks to ensure that rounding 
-- does not make the mantissa unnormalized. Rounding is necessary to prevent 
-- the bitlength of the mantissa from growing with each successive operation.
-- The input is a normalized floating-point number (e, m) with precision `P`, 
-- where `e` is a `k`-bit exponent and `m` is a `P`+1-bit mantissa.
-- The output is a normalized floating-point number (e_out, m_out) representing 
-- the same value with a lower precision `p`.""
--
-- and the Python reference implementation:
--
-- > def round_nearest_and_check(k, p, P, e, m):
-- >     ''' if mantissa >= 2^(P+1) - 2^(P-p-1), then rounding by P-p bits outputs 2^{p+1}, which is unnormalized
-- >         Thus, in this case, we increment the exponent by 1 and set the mantissa to 2^p
-- >         otherwise, we round m by P-p bits to the nearest value, i.e., \lfloor m / 2^{P-p} \rceil = (m + 2^{P-p-1}) >> P-p
-- >     '''
-- >     if m >= ((2 ** (P+1)) - (2 ** (P-p-1))):
-- >         return (e + 1, 2 ** p)
-- >     else:
-- >         shift_amt = P-p
-- >         rounded_m = (m + (2 ** (shift_amt-1))) >> shift_amt
-- >         return (e, rounded_m)
--
semantics :: TestCase -> Expected Output
semantics (e,m) = Expecting $
  if m >= 2^(bigP+1) - 2^(bigP-smallP-1)
    then (e+1, 2^smallP)
    else let shift_amt = bigP - smallP
             rounded_m = shiftR (m + 2^(shift_amt-1)) shift_amt
         in  (e,rounded_m)

testCases :: [TestCase]
testCases  
  =  [ (e,m) | e<-expoRange, m<-mantissaRange ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs (e,m) =  Inputs 
             $  toMapping "e" e
             <> toMapping "m" m

outputs :: Output -> Outputs Name Integer
outputs (e,m) =  Outputs 
              $  toMapping "e_out" e 
              <> toMapping "m_out" m

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec labCircomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------


module MOOC.Lab.CheckWellFormedness where

--------------------------------------------------------------------------------

import Data.Bits
import MOOC.Common

--------------------------------------------------------------------------------
-- global parameters

exponentBits, mantissaBits :: Int
exponentBits = 3
mantissaBits = 5

expoRange :: [Int]
expoRange = [-3..2^exponentBits+3]

mantissaRange :: [Int]
mantissaRange = [-3..2^(mantissaBits+1)+3]

mainComponent :: MainComponent
mainComponent = MainComponent 
  { _templateName   = "CheckWellFormedness"
  , _templateParams = [exponentBits, mantissaBits]
  , _publicInputs   = ["e","m"]
  }

type TestCase = (Int,Int)
type Output   = ()

--------------------------------------------------------------------------------
-- test cases and expected semantics

-- | Recall the specification:
--
--- "Enforces the well-formedness of an exponent-mantissa pair (e, m)
---  if `e` is zero, then `m` must be zero
---  else, `e` must be at most `k` bits long, and `m` must be normalized, 
--   i.e., lie in the range [2^p, 2^p+1)"
--- 
-- and the Python reference implementation:
--  
-- > def check_well_formedness(k, p, e, m):
-- >     if e == 0:
-- >         assert( m == 0 )
-- >     else:
-- >         exponent_bitcheck = (e.bit_length() <= k)
-- >         ''' To check if mantissa is in the range [2^p, 2^(p+1))
-- >             We can instead check if mantissa - 2^p is in the range [0, 2^p)
-- >         '''
-- >         tmp = m - 2 ** p
-- >         mantissa_bitcheck = tmp.bit_length() <= p
-- >         assert( exponent_bitcheck and mantissa_bitcheck )
--
semantics :: TestCase -> Expected ()
semantics (e,m) = 
  if e == 0                 
    then if m == 0 
      then Expecting () 
      else ShouldFail
    else if expoInRange e && mantissaInRange m 
      then Expecting ()
      else ShouldFail
  where
    expoInRange     e = (e >= 0             ) && (e < 2^ exponentBits    ) 
    mantissaInRange m = (m >= 2^mantissaBits) && (m < 2^(mantissaBits+1) )

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
outputs () = emptyOutputs

--------------------------------------------------------------------------------

spec :: TestSpec TestCase Output
spec = TestSpec labCircomFile mainComponent inputs outputs semantics testCases

--------------------------------------------------------------------------------

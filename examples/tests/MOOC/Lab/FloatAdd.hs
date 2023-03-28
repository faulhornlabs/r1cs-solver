
{-# LANGUAGE TypeApplications #-}
module MOOC.Lab.FloatAdd where

--------------------------------------------------------------------------------

import Data.Bits
import MOOC.Common

--------------------------------------------------------------------------------
-- global parameters

data GP = GP
  { exponentBits :: !Int
  , mantissaBits :: !Int
  }
  deriving (Eq,Show)

mainComponent :: GP -> MainComponent
mainComponent (GP exponentBits mantissaBits) = MainComponent 
  { _templateName   = "FloatAdd"
  , _templateParams = [exponentBits, mantissaBits]
  , _publicInputs   = ["e","m"]
  }

type TestCase = (MyFloat, MyFloat)
type Output   = MyFloat

--------------------------------------------------------------------------------
-- * Haskell implementation of the algorithm, for sanity checking

-- | Basics of floating-point representation
--
-- A floating-point number for the purposes of this exercise is a pair (e, m), 
-- where `e` is a `k`-bit exponent and `m` is a `p`+1-bit mantissa. `p` is called 
-- the precision of the mantissa/number.
-- The exponent `e` represents values in the range [-2^{k-1}, 2^{k-1}), but is stored 
-- as a non-negative `k`-bit integer by adding a constant `bias` = 2^{k-1} to it.
-- The mantissa `m` lies in the range [2^p, 2^p+1), i.e., its `p`-th bit is 
-- always set, and it represents the following real value: m/2^p.
-- The only exception to the mantissa range is the value 0, in which case both 
-- the exponent and mantissa are 0.
-- Overall, the value represented by a floating-point number is: m/2^p * 2^{e-bias}.
--
data MyFloat = MyFloat
  { _exponent :: !Int
  , _mantissa :: !Int
  }
  deriving (Eq,Show)

fromMyFloat :: GP -> MyFloat -> Double
fromMyFloat (GP exponentBits mantissaBits) (MyFloat e m) 
  = fromIntegral m / fromIntegral (2^mantissaBits) * 2**(fromIntegral (e-expoBias))
  where
    expoBias :: Int
    expoBias = 2^(exponentBits-1)


toMyFloat :: GP -> Double -> MyFloat
toMyFloat (GP exponentBits mantissaBits) x 
  | x <  0                    = error "toMyFloat: negative input"
  | x == 0                    = MyFloat 0 0
  | e <  0                    = error "toMyFloat: input too small to be represented"
  | e >= expoBound            = error "toMyFloat: input too large to be represented"
  | m <  2^(mantissaBits  )   = error $ "toMyFloat: fatal: mantissa is too small\n  " ++ show x ++ " ~> " ++ show (e,m)
  | m == 2^(mantissaBits+1)   = MyFloat e (m-1)
  | m >  2^(mantissaBits+1)   = error $ "toMyFloat: fatal: mantissa is too big\n  "   ++ show x ++ " ~> " ++ show (e,m)
  | otherwise                 = MyFloat e m
  where
    ef = ffloor (logBase 2 x)  :: Double
    mf = x / (2**ef)          :: Double
    e = round ef + expoBias
    m = round $ 2**(fromIntegral mantissaBits) * mf 

    expoBias :: Int
    expoBias = 2^(exponentBits-1)
 
    expoBound :: Int
    expoBound = 2^exponentBits 

ffloor :: Double -> Double
ffloor x = fromIntegral (floor x :: Int)

add :: GP -> MyFloat -> MyFloat -> MyFloat
add gp x y  = case semantics gp (x,y) of
  Expecting z  -> z
  ShouldFail   -> error "MyFloat/add: proof semantics failed"
  DontCare     -> error "MyFloat/add: this should not happen (DontCare)"

prettyMyFloat :: GP -> MyFloat -> String
prettyMyFloat gp f@(MyFloat e m)
  = show (fromMyFloat gp f) ++ " {e=" ++ show e ++ ",m=" ++ show m ++ "}"


convertPair :: GP -> (Double,Double) -> (MyFloat,MyFloat)
convertPair gp (x,y) = (toMyFloat gp x, toMyFloat gp y)

{-
instance Show MyFloat where
  show f@(MyFloat e m) = show (fromMyFloat f) ++ " {e=" ++ show e ++ ",m=" ++ show m ++ "}"

instance Num MyFloat where
  fromInteger = toMyFloat . fromInteger
  (+)         = add
  negate      = error "MyFloat/Num/negate"
  (-)         = error "MyFloat/Num/(-)"
  (*)         = error "MyFloat/Num/(*)"
  abs         = error "MyFloat/Num/abs"
  signum      = error "MyFloat/Num/signum"

instance Fractional MyFloat where
  fromRational = toMyFloat . fromRational
  recip        = error "MyFloat/Fractional/recip"
  (/)          = error "MyFloat/Fractional/(/)"

myPi = toMyFloat pi
myE  = toMyFloat (exp 1)
-}

--------------------------------------------------------------------------------
-- * expected semantics

-- | Recall the Python reference implementation
--
-- > ''' Adds two floating-point numbers.
-- >     The inputs are normalized floating-point numbers with `k`-bit exponents `e` and `p`+1-bit mantissas `m` with precision `p`.
-- >     The output is a normalized floating-point number with exponent `e_out` and a `p`+1-bit mantissa with precision `p`.
-- > '''
-- > def float_add(k, p, e_1, m_1, e_2, m_2):
-- >     ''' check that the inputs are well-formed '''
-- >     check_well_formedness(k, p, e_1, m_1)
-- >     check_well_formedness(k, p, e_2, m_2)
-- > 
-- >     ''' Arrange numbers in the order of their magnitude.
-- >         Although not the same as magnitude, note that comparing e_1 || m_1 against e_2 || m_2 suffices to compare magnitudes.
-- >     '''
-- >     mgn_1 = (e_1 << (p+1)) + m_1
-- >     mgn_2 = (e_2 << (p+1)) + m_2
-- >     ''' comparison over k+p+1 bits '''
-- >     if mgn_1 > mgn_2:
-- >         (alpha_e, alpha_m) = (e_1, m_1)
-- >         (beta_e , beta_m ) = (e_2, m_2)
-- >     else:
-- >         (alpha_e, alpha_m) = (e_2, m_2)
-- >         (beta_e , beta_m ) = (e_1, m_1)
-- > 
-- >     ''' If the difference in exponents is > p + 1, the result is alpha because the smaller value will be ignored entirely during the final rounding step.
-- >         Else, the result is the sum of the two numbers.
-- >     '''
-- >     diff = alpha_e - beta_e
-- >     if diff > p + 1 or alpha_e == 0:
-- >         ''' Simply return the larger number alpha '''
-- >         return (alpha_e, alpha_m)
-- >     else:
-- >         ''' Left-shift `alpha_m` by `diff` to align the mantissas, i.e., make the corresponding exponents equal.
-- >             Note that (e, m) and (e - diff, 2^diff * m) represent the same value.
-- >         '''
-- >         alpha_m <<= diff
-- >         ''' Add the aligned mantissas to get an unnormalized output mantissa.
-- >             The sum of the aligned mantissas `m` is guaranteed to fit in 2*p+2 bits.
-- >         ''' 
-- >         m = alpha_m + beta_m
-- >         ''' The aligned mantissa have the same exponent, i.e., `beta_e` '''
-- >         e = beta_e
-- >         ''' Now, we have an unnormalized mantissa in 2*p+2 bits with precision `p`, same as that of the input mantissas.
-- >             We need to normalize this mantissa such that it lies in the range [2^{2p+1}, 2^{2p+2}) and has precision 2p+1.
-- >             To ensure that our exponent-mantissa pair is still representing the same value, we also adjust the exponent accordingly.
-- >         '''
-- >         (normalized_e, normalized_m) = normalize(k, p, 2*p+1, e, m)
-- >         ''' Now, we have a normalized mantissa in 2*p+2 bits with precision 2p+1.
-- >             To get the same format as the inputs, we round this mantissa by p+1 bits to get a p+1-bit mantissa with precision p.
-- >         '''
-- >         (e_out, m_out) = round_nearest_and_check(k, p, 2*p+1, normalized_e, normalized_m)
-- > 
-- >         return (e_out, m_out)
--
semantics :: GP -> TestCase -> Expected MyFloat
semantics gp@(GP exponentBits mantissaBits) (MyFloat e1 m1, MyFloat e2 m2)
  | not (isWellFormed gp (e1,m1))    = ShouldFail
  | not (isWellFormed gp (e2,m2))    = ShouldFail
  | diff > p + 1 || alpha_e == 0  = Expecting (MyFloat alpha_e alpha_m)
  | otherwise                     = Expecting (MyFloat e_out   m_out  )
  where

    p = mantissaBits

    diff = alpha_e - beta_e

    m3 = shiftL alpha_m diff + beta_m
    e3 = beta_e 
    (norm_e, norm_m) = normalize p (2*p+1) (e3,m3)
    (e_out , m_out ) = roundAndCheck p (2*p+1) (norm_e, norm_m)

    ( (alpha_e, alpha_m) , (beta_e, beta_m) ) = if mgn1 > mgn2
      then ( (e1,m1) , (e2,m2) )  
      else ( (e2,m2) , (e1,m1) )

    mgn1 = fakeMagnitude (e1,m1)
    mgn2 = fakeMagnitude (e2,m2)

    -- used for comparison only
    fakeMagnitude :: (Int,Int) -> Int
    fakeMagnitude (e,m) = shiftL e (p+1) + m

    -- /// semantics of the helper functions ///

    -- (these are here because they need the global parameters, 
    -- and i don't want that as argument to all of them...)
    
    roundAndCheck :: Int -> Int -> (Int,Int) -> (Int,Int)
    roundAndCheck smallP bigP (e,m) =
      if m >= 2^(bigP+1) - 2^(bigP-smallP-1)
        then (e+1, 2^smallP)
        else let shift_amt = bigP - smallP
                 rounded_m = shiftR (m + 2^(shift_amt-1)) shift_amt
             in  (e,rounded_m)
    
    msnzb :: Int -> Int
    msnzb 0 = -1
    msnzb 1 =  0
    msnzb k =  1 + msnzb (shiftR k 1)
    
    normalize :: Int -> Int -> (Int,Int) -> (Int,Int)
    normalize smallP bigP (e,m) = 
      if (m > 0) && (m < 2^(bigP+1))   
        then let ell = msnzb m 
                 m'  = shiftL m (bigP - ell) 
                 e'  = e + ell - smallP 
             in  (e',m')
        else error "normalize: m outside of expected range"

isWellFormed :: GP -> (Int,Int) -> Bool
isWellFormed (GP exponentBits mantissaBits) (e,m) = 
  if e == 0                 
    then m == 0 
    else expoInRange e && mantissaInRange m 
  where
    expoInRange     e = (e >= 0             ) && (e < 2^ exponentBits    ) 
    mantissaInRange m = (m >= 2^mantissaBits) && (m < 2^(mantissaBits+1) )
  
--------------------------------------------------------------------------------
-- * test cases

ee :: Double
ee = exp 1 

-- testCases :: [TestCase]
-- testCases = 
--   [ ((e1,e2),(m1,m2)) 
--   | e1<-expoTestRange
--   , e2<-expoTestRange
--   , m1<-mantissaTestRange
--   , m2<-mantissaTestRange
--   ]

--------------------------------------------------------------------------------

testCases :: GP -> [TestCase]
testCases gp = case gp of
  GP 2 2 -> testCases_2_2 gp
  GP 3 2 -> testCases_3_2 gp
  GP 3 3 -> testCases_3_3 gp
  GP 3 4 -> testCases_3_4 gp
  GP 4 5 -> testCases_4_5 gp
  _ -> error $ "FloatAdd: unhandled global parameter setting " ++ show gp

-- | @GP 2 2@
--
-- NOTE: 0 + 0 takes very long time, because there are too 
-- many possible witness solutions? and we run out of memory
-- `0 + y` also but too slow but also significantly faster
-- that's why we only test these in the smallest bit setting
testCases_2_2 :: GP -> [TestCase]
testCases_2_2 gp = map (convertPair gp) pairs ++ custom where
 
  pairs  = [ (pi/2 , ee/2) 
           , (0    , pi/2) 
           , (ee/2 , 0   ) 
           , (0    , 0   ) 
           ]

  custom = [ -- overflow cases
             (MyFloat 2 7, MyFloat 2 4)
           , (MyFloat 2 7, MyFloat 3 5)
           , (MyFloat 2 7, MyFloat 1 4)
           , (MyFloat 2 7, MyFloat 2 6)
           , (MyFloat 2 7, MyFloat 3 7)

             -- difference too big cases (? - probably not, p+1 = 3)
           , (MyFloat 3 5, MyFloat 1 4)
           , (MyFloat 1 5, MyFloat 3 6)
           ]

-- | @GP 3 2@
testCases_3_2 :: GP -> [TestCase]
testCases_3_2 gp = map (convertPair gp) pairs ++ custom where

  pairs  =  [ (pi**k,ee**l) | k<-[0..0] , l<-[0..0] ]
         ++ [ (0,pi) , (ee,0) ]

  custom =  [ -- overflow cases
              (MyFloat 2 4 , MyFloat 3 7)
            , (MyFloat 3 5 , MyFloat 3 7)
            , (MyFloat 4 6 , MyFloat 3 7) 
            , (MyFloat 4 7 , MyFloat 3 5) 
---            , (MyFloat 4 7 , MyFloat 3 7)     -- takes too much time

              -- difference too big cases
            , (MyFloat 7 4  , MyFloat 2 5)       -- the solver can just handle these
            , (MyFloat 2 5  , MyFloat 7 6)       -- but they are very slow!
            ]

-- | @GP 3 3@
testCases_3_3 :: GP -> [TestCase]
testCases_3_3 gp = map (convertPair gp) pairs ++ custom where

  pairs  =  [ (pi**k,ee**l) | k<-[-1..1] , l<-[-1..1] ]
         ++ [ (0,pi) , (ee,0) ]

  custom =  [ -- overflow cases
              (MyFloat 2 9  , MyFloat 3 15)
            , (MyFloat 3 10 , MyFloat 3 15)
            , (MyFloat 4 11 , MyFloat 3 15) 
            , (MyFloat 4 14 , MyFloat 3 15) 

              -- difference too big cases
--            , (MyFloat 7 8  , MyFloat 1 10)       -- the solver can just handle these
--            , (MyFloat 1 9  , MyFloat 7 11)       -- but they are very slow!

              -- ill-formed inputs:  e==0 but m!=0
            , (MyFloat 0 10 , MyFloat 4 12)      
            , (MyFloat 3 15 , MyFloat 0 11)
            , (MyFloat 0  1 , MyFloat 4 12)
            , (MyFloat 3 15 , MyFloat 0  1)      

              -- ill-formed inputs: mantissa two small or too big
            , (MyFloat 1  0 , MyFloat 4 12)
            , (MyFloat 1  7 , MyFloat 4 12)
            , (MyFloat 1 16 , MyFloat 4 12)
            , (MyFloat 4 12 , MyFloat 2  0)
            , (MyFloat 4 12 , MyFloat 2  7)
            , (MyFloat 4 12 , MyFloat 2 16)

              -- ill-formed inputs: exponent too big
            , (MyFloat 8 10 , MyFloat 3  13)      
            , (MyFloat 8 11 , MyFloat 4  12)
            , (MyFloat 8 12 , MyFloat 5  11)
            , (MyFloat 3 13 , MyFloat 8   9)      
--            , (MyFloat 4 12 , MyFloat 8  10)     -- for some reason these take extremely long time
--            , (MyFloat 5 11 , MyFloat 8  11)
            ]

-- | @GP 3 4@
testCases_3_4 :: GP -> [TestCase]
testCases_3_4 gp = map (convertPair gp) pairs where
  s2 = sqrt 2
  s3 = sqrt 3
  hp = pi/2
  he = ee/2
  pairs =  [ (hp**k,he**l) | k<-[-2..2] , l<-[-2..2] ]
        ++ [ (s2**k,s3**l) | k<-[-2..1] , l<-[-2..1] ]
        ++ [ (ee**k,s2**l) | k<-[-2..1] , l<-[-2..1] ]
        ++ [ (s3**k,hp**l) | k<-[-2..1] , l<-[-2..1] ]

-- | @GP 4 5@
testCases_4_5 :: GP -> [TestCase]
testCases_4_5 gp = map (convertPair gp) pairs where
  s2 = sqrt 2
  s3 = sqrt 3
  hp = pi/2
  he = ee/2
  pairs =  [ (hp**k,he**l) | k<-[-2..2] , l<-[-2..2] ]
        ++ [ (s2**k,s3**l) | k<-[-2..1] , l<-[-2..1] ]
        ++ [ (he**k,s2**l) | k<-[-2..1] , l<-[-2..1] ]
        ++ [ (s3**k,hp**l) | k<-[-2..1] , l<-[-2..1] ]

--------------------------------------------------------------------------------
-- inputs and outputs

inputs :: TestCase -> Inputs Name Integer
inputs (MyFloat e1 m1 , MyFloat e2 m2) 
  =  Inputs 
  $  toMapping "e" (e1,e2)
  <> toMapping "m" (m1,m2)

outputs :: Output -> Outputs Name Integer
outputs (MyFloat e m) 
  =  Outputs 
  $  toMapping "e_out" e
  <> toMapping "m_out" m

--------------------------------------------------------------------------------

allGP :: [GP]
allGP = 
  [ GP 2 2
  , GP 3 2
  , GP 3 3
  , GP 3 4
  , GP 4 5
  ]

spec :: GP -> TestSpec TestCase Output
spec gp = TestSpec labCircomFile (mainComponent gp) inputs outputs (semantics gp) (testCases gp)

specs :: [ (GP, TestSpec TestCase Output) ]
specs = [ (gp, spec gp) | gp <- allGP ]

--------------------------------------------------------------------------------


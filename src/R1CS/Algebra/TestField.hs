
-- | The prime field with prime @p = 65537@

 module R1CS.Algebra.TestField where

--------------------------------------------------------------------------------

import Data.Int
import Data.Word
import Data.Bits
import Data.Ratio

import Data.Array

import R1CS.Algebra.SmallField
import qualified R1CS.Algebra.SmallField as Raw

--------------------------------------------------------------------------------

fieldPrime :: Int
fieldPrime = 65537

sqrtTable :: Array Word64 (Maybe F16)
sqrtTable = accumArray (flip const) Nothing (0,fromIntegral fieldPrime - 1) 
  [ (unF16 (x*x), Just x) | k<-[0..fieldPrime-1], let x = F16 (fromIntegral k) ]

fieldSqrt1 :: F16 -> Maybe F16
fieldSqrt1 (F16 k) = sqrtTable ! k

data Sqrt a 
  = NoRoot
  | DblRoot  !a
  | TwoRoots !a !a 
  deriving (Eq,Show)

sqrtToList :: Sqrt a -> [a]
sqrtToList  NoRoot        = []
sqrtToList (DblRoot  x  ) = [x]
sqrtToList (TwoRoots x y) = [x,y]

fieldSqrt :: F16 -> Sqrt F16
fieldSqrt x = case fieldSqrt1 x of
  Nothing -> NoRoot
  Just y  -> if y == 0 
    then DblRoot  y 
    else TwoRoots y (-y)

--------------------------------------------------------------------------------

-- | prime field with @p = 65537 = 2^16 + 1@
newtype F16 = F16 Word64 deriving (Eq,Show)

unF16 :: F16 -> Word64
unF16 (F16 x) = x

toF16 :: Int64 -> F16
toF16 = F16 . modF16

-- WARNING: fromInteger breaks if this is Word64, because it first converts
-- a negative integer to Word64, then takes modulo........ 
modF16 :: Int64 -> Word64
modF16 x = fromIntegral (mod x 65537)

instance Num F16 where
  fromInteger = toF16 . fromInteger
  negate (F16 x)         = F16 $ Raw.neg 65537 x
  (+)    (F16 x) (F16 y) = F16 $ Raw.add 65537 x y
  (-)    (F16 x) (F16 y) = F16 $ Raw.sub 65537 x y
  (*)    (F16 x) (F16 y) = F16 $ Raw.mul 65537 x y  
  abs    x = x
  signum _ = toF16 1  

instance Fractional F16 where
  recip (F16 x)         = F16 $ Raw.inv 65537 x
  (/)   (F16 x) (F16 y) = F16 $ Raw.div 65537 x y
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)  

isZero :: F16 -> Bool
isZero (F16 x) = (x == 0)

pow :: F16 -> Integer -> F16
pow (F16 x) n = F16 $ Raw.pow' 65537 x n

pow_ :: F16 -> Int -> F16
pow_ (F16 x) n = F16 $ Raw.pow 65537 x (fromIntegral n)

-------------------------------------------------------------------------------

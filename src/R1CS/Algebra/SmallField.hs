
-- | Small prime fields (up to @p < 2^31@), without type safety.
--
-- This module is considered internal.
--

{-# OPTIONS_HADDOCK hide #-} 
{-# LANGUAGE BangPatterns #-}
module R1CS.Algebra.SmallField where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Int
import Data.Word

--------------------------------------------------------------------------------

type P = Word64
type F = Word64 

neg :: P -> F -> F
neg !p !x = if x == 0 then x else (p - x)

add :: P -> F -> F -> F
add !p !x !y = let a = x + y in if a < p then a else (a - p)

sub :: P -> F -> F -> F
sub !p !x !y = if x >= y then (x-y) else (p+x-y)

mul :: P -> F -> F -> F
mul !p !x !y = mod (x*y) p

--------------------------------------------------------------------------------
-- * Nontrivial operations

pow :: P -> F -> Int64 -> F
pow !p !z !e 
  | z == 0    = 0
  | e == 0    = 1
  | e < 0     = pow p (inv p z) (negate e)
  | e >= pm1i = go 1 z (mod e pm1i)
  | otherwise = go 1 z e
  where
    pm1  = p - 1
    pm1i = fromIntegral pm1 :: Int64
    go :: F -> F -> Int64 -> F
    go !acc !y !e = if e == 0 
      then acc
      else case (e .&. 1) of
        0 -> go        acc    (mul p y y) (shiftR e 1)
        _ -> go (mul p acc y) (mul p y y) (shiftR e 1)

pow' :: P -> F -> Integer -> F
pow' !p !z !e 
  | e < 0     = pow' p (inv p z) (negate e)
  | z == 0    = 0
  | e == 0    = 1
  | e >= pm1  = pow  p z (fromIntegral (mod e pm1))
  | otherwise = pow  p z (fromIntegral e)
  where
    pm1 = fromIntegral (p - 1) :: Integer

-- | Inversion (using Euclid's algorithm)
inv :: P -> F -> F
inv !p !a 
  | a == 0    = error "field inverse of zero (small prime)"
  | otherwise = (euclid64 p 1 0 a p) 

-- | Division via Euclid's algorithm
div :: P -> F -> F -> F
div !p !a !b
  | b == 0    = error "field division by zero (small prime)"
  | otherwise = (euclid64 p a 0 b p) 

-- | Division via multiplying by the inverse
div2 :: P -> F -> F -> F
div2 !p !a !b = mul p a (inv p b)

--------------------------------------------------------------------------------
-- * Euclidean algorithm

-- | Extended binary Euclidean algorithm
euclid64 :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 
euclid64 !p !x1 !x2 !u !v = go x1 x2 u v where

  halfp1 = shiftR (p+1) 1

  modp :: Word64 -> Word64
  modp !n = mod n p

  -- Inverse using the binary Euclidean algorithm 
  euclid :: Word64 -> Word64
  euclid a 
    | a == 0     = 0
    | otherwise  = go 1 0 a p
  
  go :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
  go !x1 !x2 !u !v 
    | u==1       = x1
    | v==1       = x2
    | otherwise  = stepU x1 x2 u v

  stepU :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
  stepU !x1 !x2 !u !v = if even u 
    then let u'  = shiftR u 1
             x1' = if even x1 then shiftR x1 1 else shiftR x1 1 + halfp1
         in  stepU x1' x2 u' v
    else     stepV x1  x2 u  v

  stepV :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
  stepV !x1 !x2 !u !v = if even v
    then let v'  = shiftR v 1
             x2' = if even x2 then shiftR x2 1 else shiftR x2 1 + halfp1
         in  stepV x1 x2' u v' 
    else     final x1 x2  u v

  final :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
  final !x1 !x2 !u !v = if u>=v

    then let u'  = u-v
             x1' = if x1 >= x2 then modp (x1-x2) else modp (x1+p-x2)               
         in  go x1' x2  u' v 

    else let v'  = v-u
             x2' = if x2 >= x1 then modp (x2-x1) else modp (x2+p-x1)
         in  go x1  x2' u  v'

--------------------------------------------------------------------------------

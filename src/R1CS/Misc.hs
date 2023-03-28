
{-# LANGUAGE BangPatterns #-}
module R1CS.Misc where

--------------------------------------------------------------------------------

import Data.List

import Data.Map (Map) ; import qualified Data.Map.Strict as Map
import Data.Set (Set) ; import qualified Data.Set        as Set 

import Control.Concurrent.MVar

--------------------------------------------------------------------------------
-- * Same basic types (type synonyms)

-- | Name of a variable
type Name = String

-- | A prime number
type Prime = Integer

--------------------------------------------------------------------------------
-- * Bits

data Bit
  = Zero
  | One
  deriving (Eq,Ord,Show)

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One  = 1

bitToInteger :: Bit -> Integer
bitToInteger Zero = 0
bitToInteger One  = 1

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = 1

boolToInteger :: Bool -> Integer
boolToInteger False = 0
boolToInteger True  = 1

--------------------------------------------------------------------------------
-- * Verbosity

-- | Verbosity level
data Verbosity
  = Silent
  | Info
  | Verbose
  | VerbosePlus
  | Debug
  | DebugPlus
  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- * Lists

count :: (a -> Bool) -> [a] -> Int
count cond xs = length (filter cond xs)

-- | This is a hack, to help unify some tests (in particular, @mux@ in @circomlib@)
data SingletonOrList a
  = Singleton  a
  | List      [a]
  deriving (Eq,Show)

toSingletonOrList :: [a] -> SingletonOrList a
toSingletonOrList [x] = Singleton x
toSingletonOrList xs  = List xs

--------------------------------------------------------------------------------
-- * String manipulation

parens :: Bool -> String -> String
parens True  s = "(" ++ s ++ ")"
parens False s = s

--------------------------------------------------------------------------------
-- * Sets

setIsSingleton :: Ord a => Set a -> Maybe a
setIsSingleton set = case Set.toList set of
  [x] -> Just x
  _   -> Nothing

--------------------------------------------------------------------------------
-- * Either

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left  x) = Left (f x)
mapLeft _ (Right y) = Right   y

-- imapMapLeft :: (a -> b) -> IntMap (Either a c) -> IntMap (Either b c)
-- imapMapLeft f = IMap.map (mapLeft f)

mapMapLeft :: (a -> b) -> Map var (Either a c) -> Map var (Either b c)
mapMapLeft f = Map.map (mapLeft f)

--------------------------------------------------------------------------------
-- * MVar

replaceMVar :: MVar a -> a -> IO ()
replaceMVar !mvar !x = do
  _ <- tryTakeMVar mvar
  putMVar mvar x

--------------------------------------------------------------------------------


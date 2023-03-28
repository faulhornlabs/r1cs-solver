
-- | Input, output and witness

{-# LANGUAGE CPP, BangPatterns, FlexibleInstances, TypeSynonymInstances, 
             ExistentialQuantification, StandaloneDeriving 
  #-}
module R1CS.Witness where

--------------------------------------------------------------------------------

import Data.Array
import Data.List
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Maybe
import Data.Ratio
import Data.Word

import Data.Monoid
import Data.Foldable
import Data.Semigroup

import Control.Monad

import Data.Set (Set) ; import qualified Data.Set as Set
import Data.Map (Map) ; import qualified Data.Map as Map

import Data.Aeson ( toJSON )
import qualified Data.Aeson        as J
import qualified Data.Aeson.Types  as J
import qualified Data.Aeson.Key    as K
import qualified Data.Aeson.KeyMap as K

import R1CS.Constraints
import R1CS.Misc

--------------------------------------------------------------------------------
-- * Variable sets and mappings

-- | A subset of variables
newtype VarSet var 
  = VarSet (Set var)
  deriving (Eq,Show)

emptyVarSet :: VarSet var
emptyVarSet = VarSet Set.empty

appendVarSet :: Ord var => VarSet var -> VarSet var -> VarSet var
appendVarSet (VarSet set1) (VarSet set2) = VarSet (Set.union set1 set2)

-- | A mapping from variables to values
newtype Mapping var coeff 
  = Mapping (Map var coeff)
  deriving (Eq,Show)

fromMapping :: Mapping var coeff -> Map var coeff
fromMapping (Mapping table) = table

emptyMapping :: Mapping var coeff
emptyMapping = Mapping Map.empty

appendMapping :: (Ord var, Show var, Eq coeff) => Mapping var coeff -> Mapping var coeff -> Mapping var coeff
appendMapping (Mapping map1) (Mapping map2) = Mapping (Map.unionWithKey f map1 map2) where
  f key v1 v2 = if v1 == v2 
    then v1
    else error $ "Mapping/mappend: key " ++ show key ++ " would have multiple values"

instance MapVar Mapping where
  mapVar f (Mapping table) = Mapping $ Map.mapKeys f table

instance MapCoeff Mapping where
  mapCoeff f (Mapping table) = Mapping $ Map.map f table

extractMappingKeys :: Ord var => Mapping var coeff -> VarSet var
extractMappingKeys (Mapping table) = VarSet (Map.keysSet table)

-- | Note: all the variables in given set must be present in the mapping!
restrictMapping :: (Ord var, Show var) => VarSet var -> Mapping var coeff -> Mapping var coeff
restrictMapping (VarSet set) (Mapping table) = Mapping $ Map.fromList [ (key, lkp key) | key <- Set.toList set ] where
  lkp key = case Map.lookup key table of
    Just y  -> y
    Nothing -> error $ "restrictMapping: key " ++ show key ++ " not present in the mapping"

----------------------------------------
-- monoid instances

instance Ord var => Semigroup (VarSet var) where
  (<>) = appendVarSet

instance Ord var => Monoid (VarSet var) where
  mempty  = emptyVarSet
  mappend = appendVarSet

instance (Ord var, Show var, Eq coeff) => Semigroup (Mapping var coeff) where
  (<>) = appendMapping

instance (Ord var, Show var, Eq coeff) => Monoid (Mapping var coeff) where
  mempty  = emptyMapping
  mappend = appendMapping

--------------------------------------------------------------------------------
-- ** Creating mappings

class Show name => IndexedVar name where
  indexVar :: Int -> name -> name

instance IndexedVar Name where
  indexVar k name = name ++ "[" ++ show k ++ "]"

-- | Creating mappings from Haskell types
class Show t => ToMapping t where
  toMapping :: (Ord var, IndexedVar var) => var -> t -> Mapping var Integer
  hsToJson  :: t -> J.Value

instance ToMapping Bool where
  toMapping name b = Mapping $ Map.singleton name (boolToInteger b)
  hsToJson       b = toJSON  $                    (boolToInteger b)

instance ToMapping Bit where
  toMapping name b = Mapping $ Map.singleton name (bitToInteger b)
  hsToJson       b = toJSON  $                    (bitToInteger b)

instance ToMapping Int where
  toMapping name i = Mapping $ Map.singleton name (fromIntegral i)
  hsToJson       i = toJSON  $                                  i

instance ToMapping Word where
  toMapping name w = Mapping $ Map.singleton name (fromIntegral w)
  hsToJson       w = toJSON  $                                  w

instance ToMapping Word8 where
  toMapping name w = Mapping $ Map.singleton name (fromIntegral w)
  hsToJson       w = toJSON  $                                  w

instance ToMapping Word16 where
  toMapping name w = Mapping $ Map.singleton name (fromIntegral w)
  hsToJson       w = toJSON  $                                  w

instance ToMapping Word32 where
  toMapping name w = Mapping $ Map.singleton name (fromIntegral w)
  hsToJson       w = toJSON  $                                  w

instance ToMapping Word64 where
  toMapping name w = Mapping $ Map.singleton name (fromIntegral w)
  hsToJson       w = toJSON  $                                  w

instance ToMapping Integer where
  toMapping name i = Mapping $ Map.singleton name i
  hsToJson       i = toJSON  $                    i

instance (ToMapping a) => ToMapping [a] where
  toMapping name list = mconcat
    [ mapVar (indexVar i) (toMapping name x) 
    | (i,x) <- zip [0..] list
    ]
  hsToJson       list = J.toJSONList (map hsToJson list)

instance (ToMapping a) => ToMapping (SingletonOrList a) where
  toMapping name (Singleton x) = toMapping name x
  toMapping name (List   list) = mconcat
    [ mapVar (indexVar i) (toMapping name x) 
    | (i,x) <- zip [0..] list
    ]
  hsToJson       (Singleton x) = hsToJson x
  hsToJson       (List   list) = J.toJSONList (map hsToJson list)

instance (ToMapping a) => ToMapping (Array Int a) where
  toMapping name arr = toMapping name (elems arr)
  hsToJson       arr = hsToJson       (elems arr )

instance (ToMapping a) => ToMapping (a,a) where
  toMapping name (x,y) = toMapping name [x,y]
  hsToJson       (x,y) = hsToJson       [x,y]

instance (ToMapping a) => ToMapping (a,a,a) where
  toMapping name (x,y,z) = toMapping name [x,y,z]
  hsToJson       (x,y,z) = hsToJson       [x,y,z]

instance (ToMapping a) => ToMapping (a,a,a,a) where
  toMapping name (x,y,z,w) = toMapping name [x,y,z,w]
  hsToJson       (x,y,z,w) = hsToJson       [x,y,z,w]

--------------------------------------------------------------------------------
-- * Inputs, outputs and witnesses

-- | Public inputs of a circuit
newtype Inputs var coeff 
  = Inputs { fromInputs :: Mapping var coeff }
  deriving (Eq,Show)

-- | Outputs of a circuit
newtype Outputs var coeff 
  = Outputs { fromOutputs :: Mapping var coeff }
  deriving (Eq,Show)

emptyOutputs :: Outputs var coeff
emptyOutputs = Outputs $ Mapping $ Map.empty

-- | The whole witness (including the input and output)
newtype Witness var coeff 
  = Witness { fromWitness :: Mapping var coeff }
  deriving (Eq,Show)

instance MapVar Inputs  where mapVar f (Inputs  what) = Inputs  (mapVar f what)
instance MapVar Outputs where mapVar f (Outputs what) = Outputs (mapVar f what)
instance MapVar Witness where mapVar f (Witness what) = Witness (mapVar f what)

instance MapCoeff Inputs  where mapCoeff f (Inputs  what) = Inputs  (mapCoeff f what)
instance MapCoeff Outputs where mapCoeff f (Outputs what) = Outputs (mapCoeff f what)
instance MapCoeff Witness where mapCoeff f (Witness what) = Witness (mapCoeff f what)

--------------------------------------------------------------------------------

-- | The witness output from the generated WASM code / @snarkjs wej@ is just 
-- a list of integers... We also need the symbol table from the @.sym@ file
--
-- NOTE: we assume the constant 1 is included in the witness, at the very
-- first place, because it appears that's what @snarkjs wej@ generates.
mkWitness :: SymbolTable -> [Integer] -> Witness Name Integer
mkWitness symboltable values = 
  case values of
    (1:values_) -> Witness $ Mapping $ Map.fromList [ (lkpName idx, val) | (idx,val) <- zip [1..] values_ ]
    _           -> error "mkWitness: expecting a witness value list starting with the constant 1"
  where
    table = _idxLookupTable symboltable
    lkpName i = case Map.lookup i table of
      Nothing         -> error $ "mkWitness: compiled index " ++ show i ++ " not found in the symbol table"
      Just (name,loc) -> name

--------------------------------------------------------------------------------
-- * Inputs for executing the witness computation

-- | The problem: the JSON input file for witness computation uses a different format than the R1CS
-- naming convention (specifically, arrays)... We try to hack around this here
data HsInput 
  = forall t. ToMapping t => HsInput t 

deriving instance Show HsInput

newtype HsInputs 
  = HsInputs (Map Name HsInput)
  deriving Show

fromHsInputs :: HsInputs -> Map Name HsInput
fromHsInputs (HsInputs table) = table

instance Semigroup HsInputs where
  (<>) (HsInputs one) (HsInputs two) = HsInputs $ Map.union one two
  sconcat (a :| as) = HsInputs $ Map.unions $ map fromHsInputs (a:as)

instance Monoid HsInputs where
  mempty       = HsInputs $ Map.empty
  mconcat list = HsInputs $ Map.unions $ map fromHsInputs list

instance ToMapping HsInput where
  toMapping var hsinput = case hsinput of { HsInput y -> toMapping var y }
  hsToJson      hsinput = case hsinput of { HsInput y -> hsToJson      y }

hsInputsToMapping :: HsInputs -> Mapping Name Integer
hsInputsToMapping (HsInputs table) 
  = mconcat [ toMapping var inp | (var,inp) <- Map.toList table ] 

hsInputsToJSON :: HsInputs -> J.Value
hsInputsToJSON (HsInputs table) 
  = J.Object $ K.fromList 
  $ [ (K.fromString var, hsToJson inp) | (var,inp) <- Map.toList table ] 

-- ex = HsInputs $ Map.fromList 
--   [ ("foo" , HsInput [1::Int,2,3,4,5] ) 
--   , ("bar" , HsInput (One,Zero)       )
--   , ("jaj" , HsInput (12345::Integer) )
--   ]

--------------------------------------------------------------------------------

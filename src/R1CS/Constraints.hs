
-- | R1CS constraints

module R1CS.Constraints where

--------------------------------------------------------------------------------

import Data.List
import Data.Maybe
import Data.Ratio

import Control.Monad

import Data.Set (Set) ; import qualified Data.Set as Set
import Data.Map (Map) ; import qualified Data.Map as Map

import R1CS.Algebra.TestField (F16, unF16, fieldPrime)
import R1CS.Misc

--------------------------------------------------------------------------------
-- * Our \"test\" field

toField :: Rational -> Field
toField = fromRational

toField_ :: Integer -> Field
toField_ = fromInteger

toFieldConstraints :: Constraints var Rational -> Constraints var Field
toFieldConstraints (Constraints _ list) = Constraints (fromIntegral fieldPrime)
  (map (r1csMapCoeff toField) list)

type Field = F16

--------------------------------------------------------------------------------
-- * R1CS constraints

-- | A linear term with rational coefficients 
-- (where the constant term is encoded using a special variable)
newtype LinComb var coeff
  = LinComb (Map var coeff)
  deriving (Eq,Show)

-- | The R1CS constraint @a*b - c = 0@
data R1CS var coeff = R1CS 
  { _a :: !(LinComb var coeff) 
  , _b :: !(LinComb var coeff)
  , _c :: !(LinComb var coeff)
  }
  deriving (Eq,Show)

data Constraints var coeff = Constraints
  { _prime1       :: !Prime
  , _constraints1 :: ![R1CS var coeff]
  }
  deriving Show

data SymConstraints coeff = SymConstraints 
  { _constraints  :: !(Constraints VarIdx coeff)
  , _symbolTable  :: !SymbolTable
  }
  deriving Show

replacePrime :: Prime -> Constraints var coeff -> Constraints var coeff
replacePrime newPrime (Constraints _ list) = Constraints newPrime list

--------------------------------------------------------------------------------
-- ** Alternative representation of linear terms

-- | A linear term with an explicit constant coefficient. The invariant we
-- must keep is that there are no zero coefficients in the @Map@.
data LinComb' var coeff
  = LinComb' !coeff !(Map var coeff)
  deriving (Eq,Ord,Show)

linCombIsConst' :: (Ord var, Eq coeff, Num coeff) => LinComb' var coeff -> Maybe coeff
linCombIsConst' (LinComb' c table) = if all (==0) (Map.elems table) 
  then Just c
  else Nothing

-- | Enforce the inveriant
{-# SPECIALIZE normalizeLinComb' :: LinComb' VarIdx Field -> LinComb' VarIdx Field #-}
normalizeLinComb' :: (Ord var, Eq coeff, Num coeff) => LinComb' var coeff -> LinComb' var coeff 
normalizeLinComb' (LinComb' c table) = LinComb' c (Map.filter (/=0) table)

{-# SPECIALIZE linCombIsZero' :: LinComb' VarIdx Field -> Bool #-}
linCombIsZero' :: (Num coeff, Eq coeff) => LinComb' var coeff -> Bool
linCombIsZero' (LinComb' c table) = (c==0) && all (==0) (Map.elems table)

instance (Ord var, Eq coeff, Num coeff) => Num (LinComb' var coeff) where
  fromInteger x = LinComb' (fromInteger x) Map.empty
  negate (LinComb' c table) = LinComb' (negate c) (Map.map negate table)
  (+) (LinComb' c1 table1) (LinComb' c2 table2) = LinComb' (c1+c2) (Map.filter (/=0) $ Map.unionWith (+) table1 table2)
  (-) l1 l2 = l1 + negate l2
  (*)    = error "Num/LinComb': (*) not implemented"
  abs    = error "Num/LinComb': abs not implemented"
  signum = error "Num/LinComb': signum not implemented"

scaleLinComb' :: (Eq coeff, Num coeff) => coeff -> LinComb' var coeff -> LinComb' var coeff 
scaleLinComb' 0 _                  = LinComb' 0      Map.empty
scaleLinComb' u (LinComb' c table) = LinComb' (u*c) (Map.map (u*) table)

-- | We assume @0@ is the index of the special variable denoting the constant 1.
toLinComb' :: Num coeff => LinComb VarIdx coeff -> LinComb' VarIdx coeff
toLinComb' (LinComb terms) = LinComb'
  (maybe 0 id $ Map.lookup 0 terms)
  (Map.delete 0 terms)

-- | The R1CS constraint @a*b - c = 0@
data R1CS' var coeff = R1CS'
  { _a' :: !(LinComb' var coeff) 
  , _b' :: !(LinComb' var coeff)
  , _c' :: !(LinComb' var coeff)
  }
  deriving (Eq,Show)

toR1CS' :: Num coeff => R1CS VarIdx coeff -> R1CS' VarIdx coeff
toR1CS' (R1CS a b c) = R1CS' 
  (toLinComb' a)
  (toLinComb' b)
  (toLinComb' c)

--------------------------------------------------------------------------------
-- * Replacing coefficients

class MapCoeff f where
  mapCoeff :: (c1 -> c2) -> f var c1 -> f var c2  

instance MapCoeff Map         where mapCoeff = Map.map
instance MapCoeff LinComb     where mapCoeff = linCombMapCoeff
instance MapCoeff LinComb'    where mapCoeff = linCombMapCoeff'
instance MapCoeff R1CS        where mapCoeff = r1csMapCoeff
instance MapCoeff R1CS'       where mapCoeff = r1csMapCoeff'
instance MapCoeff Constraints where mapCoeff = constraintsMapCoeff

linCombMapCoeff :: (c1 -> c2) -> LinComb var c1 -> LinComb var c2
linCombMapCoeff f (LinComb terms) = LinComb $ Map.map f terms where

r1csMapCoeff :: (c1 -> c2) -> R1CS var c1 -> R1CS var c2
r1csMapCoeff f (R1CS a b c) = R1CS
  (linCombMapCoeff f a)
  (linCombMapCoeff f b)
  (linCombMapCoeff f c)

constraintsMapCoeff :: (c1 -> c2) -> Constraints var c1 -> Constraints var c2
constraintsMapCoeff f (Constraints p list) = Constraints p (map (r1csMapCoeff f) list)

linCombMapCoeff' :: (c1 -> c2) -> LinComb' var c1 -> LinComb' var c2
linCombMapCoeff' f (LinComb' c terms) = LinComb' (f c) (Map.map f terms) where

r1csMapCoeff' :: (c1 -> c2) -> R1CS' var c1 -> R1CS' var c2
r1csMapCoeff' f (R1CS' a b c) = R1CS'
  (linCombMapCoeff' f a)
  (linCombMapCoeff' f b)
  (linCombMapCoeff' f c)

----------------------------------------
-- * Replacing variables

class MapVar f where
  mapVar :: Ord v2 => (v1 -> v2) -> f v1 coeff -> f v2 coeff

instance MapVar Map         where mapVar = Map.mapKeys
instance MapVar LinComb     where mapVar = linCombMapVar
instance MapVar LinComb'    where mapVar = linCombMapVar'
instance MapVar R1CS        where mapVar = r1csMapVar
instance MapVar R1CS'       where mapVar = r1csMapVar'
instance MapVar Constraints where mapVar = constraintsMapVar

linCombMapVar :: Ord v2 => (v1 -> v2) -> LinComb v1 coeff -> LinComb v2 coeff
linCombMapVar f (LinComb terms) = LinComb $ Map.mapKeys f terms

r1csMapVar :: Ord v2 => (v1 -> v2) -> R1CS v1 coeff -> R1CS v2 coeff
r1csMapVar f (R1CS a b c) = R1CS
  (linCombMapVar f a)
  (linCombMapVar f b)
  (linCombMapVar f c)

constraintsMapVar :: Ord v2 => (v1 -> v2) -> Constraints v1 coeff -> Constraints v2 coeff
constraintsMapVar f (Constraints p list) = Constraints p (map (r1csMapVar f) list)

linCombMapVar' :: Ord v2 => (v1 -> v2) -> LinComb' v1 coeff -> LinComb' v2 coeff
linCombMapVar' f (LinComb' c terms) = LinComb' c (Map.mapKeys f terms)

r1csMapVar' :: Ord v2 => (v1 -> v2) -> R1CS' v1 coeff -> R1CS' v2 coeff
r1csMapVar' f (R1CS' a b c) = R1CS'
  (linCombMapVar' f a)
  (linCombMapVar' f b)
  (linCombMapVar' f c)

--------------------------------------------------------------------------------
-- * Symbol table mappings

-- | Index of a witness variable in the compiled R1CS
type VarIdx = Int

-- | Index of a witness variable in the original circom code 
type OrigIdx = Int

-- | Node index (whatever that means in circom)
type NodeIdx = Int

data Location = Location
  { originalIdx :: !OrigIdx            -- ^ index in the symboltable of the source code
  , compiledIdx :: !(Maybe VarIdx)     -- ^ index in the compiled circuit. Note: some variables can be optimized away!
  , nodeIdx     :: !NodeIdx            -- ^ node index (whatever that is???)
  }
  deriving (Eq,Show)

data SymbolTable = SymbolTable
  { _nameLookupTable :: !(Map Name Location)
  , _idxLookupTable  :: !(Map VarIdx (Name,Location))
  } 
  deriving (Show)

symbolTableMapName :: (Name -> Name) -> SymbolTable -> SymbolTable
symbolTableMapName f (SymbolTable nameTable idxTable) = SymbolTable nameTable' idxTable' where
  nameTable' = Map.mapKeys f nameTable
  idxTable'  = Map.map     g idxTable
  g (name, loc) = (f name, loc)

printSymbolTable :: SymbolTable -> IO ()
printSymbolTable (SymbolTable symLkp varLkp) = do
  putStrLn ""
  forM_ (Map.toList symLkp) $ \(name,loc) -> do
    putStrLn $ name ++ " @ " ++ show loc
  putStrLn ""
  forM_ (Map.toList varLkp) $ \(idx,(name,loc)) -> do
    putStrLn $ "w" ++ show idx ++ " = " ++ show name ++ " @ " ++ show loc

--------------------------------------------------------------------------------
-- * Name mangling

-- | drop the common @"main."@ prefix
dropMain :: Name -> Name
dropMain orig = if isPrefixOf "main." orig then drop 5 orig else orig where

-- | Converts to a hopefully valid Haskell identifier 
-- (useful for generating Haskell code from a circuit)
toHsName :: Name -> Name
toHsName = replaceHsKeywords . fun where
  fun = map replace . filter (/=']')
  replace '[' = '_'
  replace '.' = '_'
  replace c   = c 

replaceHsKeywords :: String -> String 
replaceHsKeywords s = if elem s hsKeywordList 
  then s ++ "_" 
  else s

hsKeywordList :: [String]
hsKeywordList = ["in","of","case","let"]

--------------------------------------------------------------------------------
-- * Coefficient mangling

recognizeRationals :: Constraints var Integer -> Constraints var Rational
recognizeRationals constr@(Constraints prime list) = constraintsMapCoeff (convertField prime) constr

convertField1 :: Prime -> Integer -> Maybe Integer
convertField1 origPrime k
  | k <= 33000 && k >= -33000   = Just $ k
  | k >  origPrime - 33000      = Just $ k - origPrime 
  | otherwise                   = Nothing -- error $ "convertField: cannot convert field element to the small field"

convertField :: Prime -> Integer -> Rational
convertField origPrime k = 
  case catMaybes [ liftM (%d) (convertField1 origPrime (mod (k*d) origPrime)) | d <- [1..maxDenom] ] of
    []    -> error $ "convertField: cannot convert field element to the small field:\n  -> " ++ show k
    (r:_) -> r
  where
    maxDenom = 17000

--------------------------------------------------------------------------------
-- * Extracting the set of variables

lincombExtractWitnessVars :: Ord var => LinComb var coeff -> Set var
lincombExtractWitnessVars (LinComb table) = Map.keysSet table

r1csExtractWitnessVars1 :: Ord var => R1CS var coeff -> Set var
r1csExtractWitnessVars1 (R1CS aa bb cc) = Set.unions
  [ lincombExtractWitnessVars aa
  , lincombExtractWitnessVars bb
  , lincombExtractWitnessVars cc
  ]

r1csExtractWitnessVars :: Ord var => [R1CS var coeff] -> Set var
r1csExtractWitnessVars list = Set.unions (map r1csExtractWitnessVars1 list)

constraintExtractWitnessVars :: Ord var => Constraints var coeff -> Set var
constraintExtractWitnessVars (Constraints p list) = r1csExtractWitnessVars list

lincombExtractWitnessVars' :: Ord var => LinComb' var coeff -> Set var
lincombExtractWitnessVars' (LinComb' _ table) = Map.keysSet table

r1csExtractWitnessVars3' :: Ord var => R1CS' var coeff -> (Set var, Set var, Set var)
r1csExtractWitnessVars3' (R1CS' aa bb cc) = 
  ( lincombExtractWitnessVars' aa
  , lincombExtractWitnessVars' bb
  , lincombExtractWitnessVars' cc
  )

--------------------------------------------------------------------------------
-- * Pretty printing

data Printer var coeff = Printer 
  { _varPrinter   :: var   -> String
  , _coeffPrinter :: coeff -> String 
  }

mkSymbolicPrinter :: (coeff -> String) -> SymbolTable -> Printer VarIdx coeff
mkSymbolicPrinter showCoeff (SymbolTable nameTable idxTable) = Printer showVar showCoeff where
  showVar 0   = "1"
  showVar idx = fst $ fromJust $ Map.lookup idx idxTable

symbolicRatPrinter :: SymbolTable -> Printer VarIdx Rational
symbolicRatPrinter sym = mkSymbolicPrinter prettyRational sym

symbolicFieldPrinter :: SymbolTable -> Printer VarIdx Field
symbolicFieldPrinter sym = mkSymbolicPrinter prettyField sym

prettyVarIdx :: VarIdx -> String
prettyVarIdx 0 = "1"
prettyVarIdx i = "w" ++ show i

idxRatPrinter :: Printer VarIdx Rational
idxRatPrinter = Printer prettyVarIdx prettyRational 

----------------------------------------

prettyRational :: Rational -> String
prettyRational r = 
  parens (a < 0) $ if b == 1
    then show a
    else show a ++ "/" ++ show b
  where
    a = numerator   r
    b = denominator r

prettyField :: Field -> String
prettyField = show . unF16

----------------------------------------

class PrettyPrint f where
  prettyPrint :: Printer var coeff -> f var coeff -> String

instance PrettyPrint LinComb     where prettyPrint = prettyLinComb
instance PrettyPrint LinComb'    where prettyPrint = prettyLinComb'
instance PrettyPrint R1CS        where prettyPrint = prettyR1CS
instance PrettyPrint R1CS'       where prettyPrint = prettyR1CS'
instance PrettyPrint Constraints where prettyPrint = prettyConstraints

prettyTerm :: Printer var coeff -> (var,coeff) -> String
prettyTerm (Printer showVar showCoeff) (v,c) 
  | s == "" || s == "1"  = t
  | t == "" || t == "1"  = s
  | otherwise            = t ++ "*" ++ s
  where
    s  = showVar   v
    t0 = showCoeff c
    t  = case t0 of
      ('_':rest) -> parens True t0
      _          -> t0

prettyLinComb :: Printer var coeff -> LinComb var coeff -> String
prettyLinComb printer (LinComb lc)  
  | Map.null lc   = "0"
  | otherwise     = "[ " ++ intercalate " + " (map (prettyTerm printer) (Map.toList lc)) ++ " ]"

prettyLinComb' :: Printer var coeff -> LinComb' var coeff -> String
prettyLinComb' printer (LinComb' c0 lc)  
  | Map.null lc   = "[ " ++ _coeffPrinter printer c0 ++  "]"
  | otherwise     = "[ " ++ intercalate " + " ( _coeffPrinter printer c0 : map (prettyTerm printer) (Map.toList lc)) ++ " ]"

prettyR1CS :: Printer var coeff -> R1CS var coeff -> String
prettyR1CS printer (R1CS a b c) =
  prettyLinComb printer a ++ " * " ++ 
  prettyLinComb printer b ++ " - " ++ 
  prettyLinComb printer c ++ " == 0"

prettyR1CS' :: Printer var coeff -> R1CS' var coeff -> String
prettyR1CS' printer (R1CS' a b c) =
  prettyLinComb' printer a ++ " * " ++ 
  prettyLinComb' printer b ++ " - " ++ 
  prettyLinComb' printer c ++ " == 0"

prettyConstraints :: Printer var coeff -> Constraints var coeff -> String
prettyConstraints printer (Constraints prime list) = unlines (header : map (prettyR1CS printer) list) where
  header = "prime = " ++ show prime

printConstraints :: Printer var coeff -> Constraints var coeff -> IO ()
printConstraints printer what = do
  putStrLn ""
  putStrLn (prettyConstraints printer what)

printSymConstraints :: (coeff -> String) -> SymConstraints coeff -> IO ()
printSymConstraints showCoeff (SymConstraints constr sym) = 
  printConstraints (mkSymbolicPrinter showCoeff sym) constr

--------------------------------------------------------------------------------

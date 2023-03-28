
{-# LANGUAGE BangPatterns, TypeApplications, PatternSynonyms, TypeSynonymInstances, 
             GeneralizedNewtypeDeriving, StandaloneDeriving 
  #-}
module R1CS.Solver where

--------------------------------------------------------------------------------

import Data.Ord
import Data.List
import Data.Maybe

import Control.Monad
import Control.Monad.State.Strict

import qualified Data.Foldable as F

import Data.Set    (Set)    ; import qualified Data.Set           as Set
import Data.Map    (Map)    ; import qualified Data.Map.Strict    as Map
-- import Data.IntSet (IntSet) ; import qualified Data.IntSet        as ISet
-- import Data.IntMap (IntMap) ; import qualified Data.IntMap.Strict as IMap

import R1CS.Constraints
import R1CS.Algebra.TestField
import R1CS.Misc

--------------------------------------------------------------------------------

-- required for some Ord instances used for sorting things
deriving instance Ord F16

--------------------------------------------------------------------------------

{-
data Restricted a
  = Fixed    !a
  | Possible [a]
  | Unrestricted
  deriving (Eq,Show)

pattern Impossible = Possible []

type Solution key = Map key (Restricted Field)
-}

--------------------------------------------------------------------------------

listUnknownNames :: SymbolTable -> [Name] -> [Name]
listUnknownNames (SymbolTable nameTable idxTable) list = output where
  output = [ n | n <- list, isNothing (nameLkp n) ]
  nameLkp name = compiledIdx =<< Map.lookup name nameTable

--------------------------------------------------------------------------------

solver :: Verbosity -> SymConstraints Field -> Map Name Field -> IO [Solution Name Field]
solver verbosity (SymConstraints constraints symboltable) initial = 

  do
    unless (prime == 65537) $ error "expecting an R1CS over the prime field with p=65537"

    case listUnknownNames symboltable (Map.keys initial) of 
      []   -> return ()
      list -> error $ "the following names in the input are not present in the symbol table:\n  " ++ show list

    when (Map.null initial) $ error "initial condition is empty"

    when (verbosity >= Verbose) $ do
      putStrLn "initial condition:"
      forM_ (Map.toList initial) $ \(name,val) -> do
        putStrLn $ name ++ " <== " ++ show val

    when (verbosity >= Debug) $ do
      putStrLn $ "starting substitution: " ++ show startingSubs
      
    --  when (verbosity >= DebugPlus) $ do
    --    putStrLn "==========\noriginal r1cs:"
    --    mapM_ print r1cs
    --    putStrLn "==========\nr1cs':"
    --    mapM_ print $ map toR1CS' r1cs
    --    putStrLn "==========\nstarting constraints:"
    --    mapM_ print startingConstr

    let results    = runYield (solverStep startingPartial_)
    let solutions  = map (analyzeResult varSet) results

    -- print varSet
    -- mapM_ printResult results

    when (verbosity >= Verbose) $ do
      putStrLn $ "number of 'solution templates' = " ++ show (length solutions)

    return $ map (mapVar unsafeIdxLkp) solutions

  where

    SymbolTable nameTable idxTable = symboltable
    Constraints prime     r1cs     = constraints

    nameLkp :: Name -> Maybe VarIdx
    nameLkp name = compiledIdx =<< Map.lookup name nameTable

    idxLkp :: VarIdx -> Maybe Name
    idxLkp idx = fst <$> Map.lookup idx idxTable
 
    unsafeIdxLkp :: VarIdx -> Name
    unsafeIdxLkp idx = case idxLkp idx of
      Just name -> name
      Nothing   -> error $ "fatal error: variable index " ++ show idx ++ " not found in the symbol table"

    varSet :: Set VarIdx
    varSet = Set.delete 0                   -- note: 0 is used to encode the special variable "1"
           $ r1csExtractWitnessVars r1cs

    startingSubs :: Map VarIdx Field
    startingSubs = Map.fromList 
      [ ( fromJust (nameLkp name) , val ) -- fromInteger val ) 
      | (name,val) <- Map.toList initial 
      ]

    startingRestr :: Restrictions_
    startingRestr = Map.map Right startingSubs

    startingConstr :: [R1CS' VarIdx Field]
    startingConstr = substituteConstraints startingSubs $ map toR1CS' r1cs

    startingPartial_ :: Partial_
    startingPartial_ = Partial startingRestr startingConstr

--------------------------------------------------------------------------------
-- * constant substitutions

class Substitute1 f where
  substitute1    :: (Ord var, Eq coeff, Num coeff) => (var,coeff)   -> f var coeff -> f var coeff
  substituteMany :: (Ord var, Eq coeff, Num coeff) => Map var coeff -> f var coeff -> f var coeff
  substituteMany subs old = foldr substitute1 old (Map.toList subs)

instance Substitute1 LinComb' where
  substitute1    = substituteLinComb1
  substituteMany = substituteLinComb
instance Substitute1 R1CS' where
  substitute1    = substituteR1CS_1
  substituteMany = substituteR1CS

{-# SPECIALIZE substituteLinComb :: Map VarIdx Field -> LinComb' VarIdx Field -> LinComb' VarIdx Field #-}
substituteLinComb :: (Ord var, Num a) => Map var a -> LinComb' var a -> LinComb' var a
substituteLinComb subsTable (LinComb' c0 terms) = LinComb' c0' remaining where
  c0' = c0 + sum [ cf * subs | (key,cf) <- Map.toList terms , subs <- F.toList (Map.lookup key subsTable) ]
  remaining = Map.filterWithKey (\key _ -> not (Map.member key subsTable)) terms 

{-# SPECIALIZE substituteR1CS :: Map VarIdx Field -> R1CS' VarIdx Field -> R1CS' VarIdx Field #-}
substituteR1CS :: (Ord var, Num a) => Map var a -> R1CS' var a -> R1CS' var a
substituteR1CS subs (R1CS' a b c) = R1CS'
  (substituteLinComb subs a)
  (substituteLinComb subs b)
  (substituteLinComb subs c)

{-# SPECIALIZE substituteConstraints :: Map VarIdx Field -> [R1CS' VarIdx Field] -> [R1CS' VarIdx Field] #-}
substituteConstraints :: (Ord var, Num a) => Map var a -> [R1CS' var a] -> [R1CS' var a]
substituteConstraints subs = map (substituteR1CS subs) 

----------------------------------------

{-# SPECIALIZE substituteLinComb1 :: (VarIdx,Field) -> LinComb' VarIdx Field -> LinComb' VarIdx Field #-}
substituteLinComb1 :: (Ord var, Num a) => (var,a) -> LinComb' var a -> LinComb' var a
substituteLinComb1 (x,val) lc@(LinComb' c0 terms) = case Map.lookup x terms of
  Nothing -> lc
  Just u  -> LinComb' (c0 +  u*val) (Map.delete x terms)

{-# SPECIALIZE substituteR1CS_1 :: (VarIdx,Field) -> R1CS' VarIdx Field -> R1CS' VarIdx Field #-}
substituteR1CS_1 :: (Ord var, Num a) => (var,a) -> R1CS' var a -> R1CS' var a
substituteR1CS_1 subs (R1CS' a b c) = R1CS'
  (substituteLinComb1 subs a)
  (substituteLinComb1 subs b)
  (substituteLinComb1 subs c)

{-# SPECIALIZE substituteConstraints1 :: (VarIdx,Field) -> [R1CS' VarIdx Field] -> [R1CS' VarIdx Field] #-}
substituteConstraints1 :: (Ord var, Num a) => (var,a) -> [R1CS' var a] -> [R1CS' var a]
substituteConstraints1 subs = map (substituteR1CS_1 subs) 

--------------------------------------------------------------------------------
-- * linear substitutions

class SubstituteLC f where
  substituteLC :: (Ord var, Eq coeff, Num coeff) => (var, LinComb' var coeff) -> f var coeff -> f var coeff

instance SubstituteLC LinComb' where substituteLC = linearSubsLinComb
instance SubstituteLC R1CS'    where substituteLC = linearSubsR1CS

{-# SPECIALIZE linearSubsLinComb :: (VarIdx, LinComb' VarIdx Field) -> LinComb' VarIdx Field -> LinComb' VarIdx Field #-}
linearSubsLinComb :: (Ord var, Eq a, Num a) => (var, LinComb' var a) -> LinComb' var a -> LinComb' var a
linearSubsLinComb (x, what) lc@(LinComb' c table) = 
  case Map.lookup x table of
    Nothing -> lc1
    Just u  -> lc1 + scaleLinComb' u what
  where
    lc1 = LinComb' c (Map.delete x table) 
           
{-# SPECIALIZE linearSubsR1CS :: (VarIdx, LinComb' VarIdx Field) -> R1CS' VarIdx Field -> R1CS' VarIdx Field #-}
linearSubsR1CS :: (Ord var, Eq a, Num a) => (var, LinComb' var a) -> R1CS' var a -> R1CS' var a
linearSubsR1CS pair (R1CS' aa bb cc) = R1CS'
  (linearSubsLinComb pair aa)
  (linearSubsLinComb pair bb)
  (linearSubsLinComb pair cc)

{-# SPECIALIZE linearSubsConstraints :: (VarIdx, LinComb' VarIdx Field) -> [R1CS' VarIdx Field] -> [R1CS' VarIdx Field] #-}
linearSubsConstraints :: (Ord var, Eq a, Num a) => (var, LinComb' var a) -> [R1CS' var a] -> [R1CS' var a]
linearSubsConstraints subs = map (linearSubsR1CS subs) 

--------------------------------------------------------------------------------
-- * Yield monad to collect solutions

newtype Yield y a 
  = Yield (State [y] a)
  deriving (Functor, Applicative,Monad)

yield :: y -> Yield y ()
yield !what = Yield $ do
  old <- get
  put (what:old)   

runYield :: Yield y () -> [y]
runYield (Yield action) = execState action []

--------------------------------------------------------------------------------
-- * Table of substitutions we keep while solving

-- | TODO: replace this with the isomorphic 'Substitution'
type Restrictions var coeff
  = Map var (Either (LinComb' var coeff) coeff)

restrictionsMapVar :: Ord var2 => (var1 -> var2) -> Restrictions var1 coeff -> Restrictions var2 coeff
restrictionsMapVar f table = Map.fromList $ map g $ Map.toList table where
  g (key,rhs) = (f key, mapLeft (mapVar f) rhs)

type Restrictions_ = Restrictions VarIdx Field

printRestrictions_ :: Restrictions_ -> IO ()
printRestrictions_ table = 
  forM_ (Map.toList table) $ \(idx,ei) -> do
    putStr $ "  w" ++ show idx ++ " ~> " 
    case ei of
      Right (F16 y) -> putStrLn $ show y
      Left  lc      -> putStrLn $ prettyLinComb' printer lc
  where
    printer = Printer prettyVarIdx prettyField

printRestrictions' :: Printer var coeff -> Restrictions var coeff -> IO ()
printRestrictions' printer@(Printer showVar showCoeff) table = 
  forM_ (Map.toList table) $ \(key,ei) -> do
    putStr $ "  " ++ showVar key ++ " ~> " 
    case ei of
      Right y  -> putStrLn $ showCoeff y
      Left  lc -> putStrLn $ prettyLinComb' printer lc

normalizeRestrictions :: (Ord var, Eq coeff, Num coeff) => Restrictions var coeff -> Restrictions var coeff
normalizeRestrictions = Map.map fun where
  fun (Right y) = Right y
  fun (Left lc) = case linCombIsConst' lc of 
    Just y  -> Right y
    Nothing -> Left lc

{-# SPECIALIZE subsIntoRestrictions1 :: (VarIdx,Field) -> Restrictions_ -> Restrictions_ #-}
subsIntoRestrictions1 :: (Ord var, Eq coeff, Num coeff) => (var,coeff) -> Restrictions var coeff -> Restrictions var coeff
subsIntoRestrictions1 (x,value) table = 
  case Map.lookup x table of
    Nothing -> normalizeRestrictions $ 
               Map.insert x (Right value) $ 
               mapMapLeft (substituteLinComb1 (x,value)) $ 
               table
    Just _  -> error "subsIntoRestrictions: key already present"

{-# SPECIALIZE linearSubsRestrictions :: (VarIdx, LinComb' VarIdx Field) -> Restrictions_ -> Restrictions_ #-}
linearSubsRestrictions :: (Ord var, Eq coeff, Num coeff) => (var, LinComb' var coeff) -> Restrictions var coeff -> Restrictions var coeff
linearSubsRestrictions (x,lc) table = 
  case linCombIsConst' lc of
    Just y  -> subsIntoRestrictions1 (x,y) table
    Nothing -> case Map.lookup x table of
      Nothing -> normalizeRestrictions $
                 Map.insert x (Left lc) $ 
                 mapMapLeft (linearSubsLinComb (x,lc)) $ 
                 table
      Just _  -> error "linearSubsRestrictions: key already present"

--------------------------------------------------------------------------------
-- * Substitutions

newtype Substitution var coeff 
  = Substitution (Map var (RHS var coeff))
  deriving Show

restrictionToSubstitution :: Restrictions var coeff -> Substitution var coeff
restrictionToSubstitution restr = Substitution (Map.map f restr) where
  f (Left  lc ) = LinearExp lc
  f (Right val) = Value     val

-- | The right-hand side of a subsitution
data RHS var coeff
  = Value     coeff                  -- ^ a concrete value
  | LinearExp (LinComb' var coeff)   -- ^ a linear expression
  deriving Show

rhsIsValue :: RHS var coeff -> Bool
rhsIsValue (Value y) = True
rhsIsValue _         = False

rhsMbValue :: RHS var coeff -> Maybe coeff
rhsMbValue (Value y) = Just y
rhsMbValue _         = Nothing

instance MapVar RHS where
  mapVar _ (Value y) = Value y
  mapVar f (LinearExp lc) = LinearExp (mapVar f lc) 

instance MapVar Substitution where
  mapVar f (Substitution table) = Substitution $ Map.fromList $ map g $ Map.toList table where
    g (key,rhs) = (f key, mapVar f rhs)

printSubstitution :: Printer var coeff -> Substitution var coeff -> IO ()
printSubstitution printer@(Printer showVar showCoeff) (Substitution table) = 
  forM_ (Map.toList table) $ \(key,rhs) -> do
    putStr $ "  " ++ showVar key ++ " ~> " 
    case rhs of
      Value     y  -> putStrLn $ showCoeff y
      LinearExp lc -> putStrLn $ prettyLinComb' printer lc

--------------------------------------------------------------------------------
-- * Solutions

-- | A final solution (can be several of such!)
data Solution var coeff
  = UniqueSolution   !(Map var coeff)
  | HasFreeVariables !(Set var) !(Substitution var coeff)
  | CouldntSolve     !(Partial var coeff)
  deriving Show

type Solutions var coeff = [Solution var coeff]

printSolution :: Solution Name Field -> IO ()
printSolution sol = 
  case sol of

    UniqueSolution restr -> do
      putStrLn "finished 0-dimensional solution:"
      printUniqueSolution printer restr

    HasFreeVariables free restr -> do
      putStrLn "finished high dimensional solution:"
      printSubstitution printer restr
      putStrLn "remaining free variables:"
      forM_ (Set.toList free) $ \var -> putStrLn ("  " ++ var)

    CouldntSolve partial@(Partial restr constr) -> do
      putStrLn "unfinished solution:"
      printRestrictions' printer restr
      putStrLn "remaining equations:"
      forM_ constr $ \r1cs -> 
        putStrLn $ "  " ++ prettyR1CS' printer r1cs

  where
    printer = Printer id prettyField

printUniqueSolution :: Printer var coeff -> Map var coeff -> IO ()
printUniqueSolution (Printer showVar showCoeff) table = do
  forM_ (Map.toList table) $ \(var,value) -> putStrLn ("  " ++ showVar var ++ " ~> " ++ showCoeff value)

instance MapVar Solution where
  mapVar f (CouldntSolve     partial  ) = CouldntSolve     (mapVar  f partial)
  mapVar f (UniqueSolution   table    ) = UniqueSolution   (mapVar  f table  )
  mapVar f (HasFreeVariables free subs) = HasFreeVariables (Set.map f free   ) (mapVar f subs   )

-- instance MapVar Solutions where
--   mapVar f = map (mapVar f)

analyzeResult :: Set VarIdx -> Result -> Solution VarIdx Field
analyzeResult varSet result = 
  case result of
    Unfinished partial -> 
      CouldntSolve partial
    Finished restr   -> 
      let freeVars = Set.difference varSet (Map.keysSet restr) in
      if Set.null freeVars 
        then UniqueSolution   (Map.map onlyRight restr)
        else HasFreeVariables freeVars (restrictionToSubstitution restr)
  where
    onlyRight (Right y) = y
    onlyRight (Left  _) = error "analyzeResult: no free variables but linear subsitution found (should not happen)"

----------------------------------------

-- | A single result of the solver
data Result
  = Finished   !Restrictions_      -- ^ we eliminated all equations (but there could be free variables!)
  | Unfinished !Partial_           -- ^ we couldn't eliminate all equations (solver didn't finish)
  deriving Show

printResult :: Result -> IO ()
printResult result = 
  case result of
    Finished restr -> do
      putStrLn "finished (but not necessarily 0-dimensional) solution:"
      printRestrictions_ restr
    Unfinished partial@(Partial restr constr) -> do
      putStrLn "unfinished solution:"
      printRestrictions_ restr
      putStrLn "remaining equations:"
      forM_ constr $ \r1cs -> 
        putStrLn $ " " ++ prettyR1CS' printer r1cs
  where
    printer = Printer prettyVarIdx prettyField

--------------------------------------------------------------------------------
-- * A single step of the solver

-- | A partial solution
data Partial var coeff = Partial 
  { _restrictions :: Restrictions var coeff
  , _pconstraints :: [R1CS' var coeff]
  }
  deriving Show

instance MapVar Partial where
  mapVar f (Partial restr constr) = Partial 
    (restrictionsMapVar f restr) 
    (map (mapVar f) constr)

type Partial_ = Partial VarIdx Field

-- | Go through all equations, try to solve them individually; collect the results
-- and tries to simplify the system
solverStep :: Partial_ -> Yield Result ()
solverStep partial@(Partial restrictions constraints) 
  | is_there_contradiction  = return ()
  | null actionList         = yield $ Finished   restrictions
  | only_postponings        = yield $ Unfinished partial
  | otherwise               = proceed
  where
    unsortedRows = [ (con,act) | con <- constraints, let act = checkSingleEquation con, act /= Single TrivialEq ]
    sortedRows   = sortBy (comparing snd) unsortedRows
    actionList   = map snd sortedRows
    is_there_contradiction = any (==Single Contradiction) actionList
    only_postponings       = all (==Single Postpone     ) actionList

    proceed = case sortedRows of
      [] -> error "solverStep/proceed: should not happen"
      (_con,actions) : rest -> let rest_ = map fst rest in case actions of

        Single action -> do
          solverStep $ applyAction action  (Partial restrictions rest_)

        Branch action1 action2 -> do
          solverStep $ applyAction action1 (Partial restrictions rest_)
          solverStep $ applyAction action2 (Partial restrictions rest_)

applyAction :: Action -> Partial_ -> Partial_
applyAction action (Partial restrictions constraints) = case action of
  
  SolveWith x val -> Partial (subsIntoRestrictions1  (x,val) restrictions)
                             (substituteConstraints1 (x,val) constraints )

  Eliminate x lc  -> Partial (linearSubsRestrictions (x,lc ) restrictions)
                             (linearSubsConstraints  (x,lc ) constraints )

  TrivialEq       -> error "applyAction: TrivialEq (should not happen)"
  Postpone        -> error "applyAction: Postpone (should not happen)"
  Contradiction   -> error "applyAction: Contradiction (should not happen)" 

--------------------------------------------------------------------------------
-- * solving a single equation

-- | Note: The order is important here! We sort the equations based on this
data Action 
  = Contradiction                                  -- ^ no possible solutions
  | SolveWith !VarIdx !Field                       -- ^ substitute a single constant value
  | Eliminate !VarIdx !(LinComb' VarIdx Field)     -- ^ eliminate by a linear substitution 
  | TrivialEq                                      -- ^ trivial equation, can be dropped
  | Postpone                                       -- ^ postpone solving this equation
  deriving (Eq,Ord,Show)

data Actions
  = Single !Action
  | Branch !Action Action
  deriving (Eq,Show)

instance Ord Actions where
  compare (Single Postpone) (Single Postpone) = EQ
  compare (_              ) (Single Postpone) = LT
  compare (Single Postpone) (_              ) = GT
  compare (Single a       ) (Single b       ) = compare a b
  compare (Single a       ) (Branch _ _     ) = LT
  compare (Branch _ _     ) (Single b       ) = GT
  compare (Branch a1 a2   ) (Branch b1 b2   ) = compare (a1,a2) (b1,b2)

-- | create an \"optimized\" branching: For example if one branch would result
-- in contradiction, we don't have to follow that; if one branch would result in
-- just postponing, we don't have to follow that either; but if one branch is
-- automatically satisfied because it's trivial, then we don't have to follow 
-- /the other/ (this happens when one term of a product is constant zero, making
-- the whole constraint trivial)
branch :: Action -> Action -> Actions
branch action1 action2 = case (action1, action2) of
  ( Contradiction , act2          ) -> Single act2
  ( act1          , Contradiction ) -> Single act1
  ( TrivialEq     , act2          ) -> Single TrivialEq 
  ( act1          , TrivialEq     ) -> Single TrivialEq
  ( Postpone      , act2          ) -> Single act2
  ( act1          , Postpone      ) -> Single act1
  ( act1          , act2          ) -> if act1 /= act2 
                                         then Branch act1 act2 
                                         else Single act1

--------------------------------------------------------------------------------

checkSingleEquation :: R1CS' VarIdx Field -> Actions
checkSingleEquation r1cs@(R1CS' aa bb cc) 
  | linCombIsZero' aa || linCombIsZero' bb = Single (solveLinearEquation cc)    -- linear equation: c = 0
  | Just a <- linCombIsConst' aa           = Single $ solveLinearEquation       -- linear equation, because a is constant
                                                    $ scaleLinComb' a bb - cc   
  | Just b <- linCombIsConst' bb           = Single $ solveLinearEquation       -- linear equation, because b is constant
                                                    $ scaleLinComb' b aa - cc
  | linCombIsZero' cc                      = productIsZero aa bb                -- a*b = 0  =>  a=0 or b=0
  | Just v <- setIsSingleton varsABC       = quadraticUnivariate r1cs           -- quadratic equation in a single variable
  | Set.null varsABC                       = Single (noFreeVariables r1cs)      -- no free variables, check whether the
  | otherwise                              = Single Postpone                    -- postpone this equation
  where 
    (varsA,varsB,varsC) = r1csExtractWitnessVars3' r1cs  
    varsABC = Set.unions [varsA,varsB,varsC]

solveLinearEquation :: LinComb' VarIdx Field -> Action
solveLinearEquation lincomb@(LinComb' c linear) = 
  case Set.toList (lincombExtractWitnessVars' lincomb) of
    []       -> if c == 0 then TrivialEq else Contradiction
    [x]      -> let a = (Map.!) linear x in
                if a == 0 
                  then if c == 0 
                    then TrivialEq                    -- 0*x + 0 == 0
                    else Contradiction                -- 0*x + c == 0
                  else SolveWith x (negate c / a)     -- a*x + c == 0
    (x:rest) -> let a = (Map.!) linear x in
                if a == 0 
                  then error "solveLinearEquation: fatal: coeff is 0"
                  else let lc' = negate $ LinComb' (c/a) (Map.map (/a) $ Map.delete x linear) in
                       Eliminate x lc'

productIsZero :: LinComb' VarIdx Field -> LinComb' VarIdx Field -> Actions
productIsZero aa bb = branch (solveLinearEquation aa) (solveLinearEquation bb) 

quadraticUnivariate :: R1CS' VarIdx Field -> Actions
quadraticUnivariate r1cs 
  | a == 0 && b == 0   = Single $ if c == 0 then TrivialEq else Contradiction
  | a == 0             = Single $ SolveWith x (-c / b)
  | otherwise          = actions
  where
    (x,quadratic)        = toUniQuadraticEq  r1cs
    UniQuadraticEq a b c = quadratic
    sol                  = solveUniQuadratic quadratic
    actions              = case sqrtToList sol of 
      []    -> Single Contradiction
      [u]   -> Single (SolveWith x u)
      [u,v] -> Branch (SolveWith x u) (SolveWith x v)

noFreeVariables :: R1CS' VarIdx Field -> Action
noFreeVariables (R1CS' (LinComb' a linA)
                       (LinComb' b linB)
                       (LinComb' c linC)) 
  | Map.null linA && Map.null linB && Map.null linC = if (a*b - c == 0) then TrivialEq else Contradiction
  | otherwise = error "Solver/noFreeVariables: fatal error: there are free variables" 

--------------------------------------------------------------------------------
-- ** Solving univariate equations

-- | The univariate linear equation @a*x + b = 0@
data UniLinearEq 
  = UniLinearEq !Field !Field 
  deriving Show

-- | The univariate quadratic equation @a*x^2 + b*x+c = 0@
data UniQuadraticEq 
  = UniQuadraticEq !Field !Field !Field
  deriving Show

toUniLinearEq :: LinComb' VarIdx Field -> (Maybe VarIdx, UniLinearEq)
toUniLinearEq (LinComb' b table) = case Map.toList table of
  []      -> (Nothing, UniLinearEq 0 b)
  [(x,a)] -> (Just x , UniLinearEq a b)
  _       -> error "toUniLinearEq: input is not univariate"

toUniQuadraticEq :: R1CS' VarIdx Field -> (VarIdx, UniQuadraticEq)
toUniQuadraticEq (R1CS' lcA lcB lcC) = 
  case nub $ catMaybes [mbX,mbY,mbZ] of 
    [x] -> (x, quadratic)
    _   -> error "toUniQuadraticEq: input is not univariate"
  where
    (mbX, UniLinearEq a1 a0) = toUniLinearEq lcA
    (mbY, UniLinearEq b1 b0) = toUniLinearEq lcB
    (mbZ, UniLinearEq c1 c0) = toUniLinearEq lcC
    quadratic = UniQuadraticEq (a1*b1) (a1*b0 + a0*b1 - c1) (a0*b0 - c0)

solveUniQuadratic :: UniQuadraticEq -> Sqrt Field
solveUniQuadratic (UniQuadraticEq a b c) =
  case fieldSqrt discr of
    NoRoot       -> NoRoot
    DblRoot  z   -> if z == 0 
                      then DblRoot (-b / twoA) 
                      else error "solveUniQuadratic: fatal error"
    TwoRoots u v -> TwoRoots ((-b + u)/twoA) ((-b + v)/twoA)
  where
    discr = b*b - 4*a*c
    twoA  = a+a

--------------------------------------------------------------------------------


-- | Run soundness checks on given inputs

{-# LANGUAGE ScopedTypeVariables, DeriveFunctor #-}
module R1CS.Test.Runner where

--------------------------------------------------------------------------------

import Data.Bits

import Control.Monad

import Data.Set (Set) ; import qualified Data.Set        as Set
import Data.Map (Map) ; import qualified Data.Map.Strict as Map

import R1CS.Constraints
import R1CS.Witness
import R1CS.Solver
import R1CS.Compile
import R1CS.Misc

-- import R1CS.Parser.Constraint.Text ( parseTextFile     )
import R1CS.Parser.Constraint.JSON ( parseJsonFile     )
import R1CS.Parser.SymFile         ( parseSymFile      )
import R1CS.Parser.WtnsJSON        ( parseWtnsJsonFile )

--------------------------------------------------------------------------------

data Expected a
  = Expecting !a         -- ^ expected output
  | OneOf     [a]        -- ^ the output should be a member of this set
  | DontCare             -- ^ we don't care about the output, but the proof should succeed
  | ShouldFail           -- ^ the proof should fail
  deriving (Show,Functor)

--------------------------------------------------------------------------------

data TestResult 
  = Passed 
  | Failed
  | Unsolved
  deriving (Eq,Show)

didPass, didFail, wasUnsolved  :: TestResult -> Bool
didPass     res = case res of { Passed   -> True ; _ -> False }
didFail     res = case res of { Failed   -> True ; _ -> False }
wasUnsolved res = case res of { Unsolved -> True ; _ -> False }

--------------------------------------------------------------------------------

analyzeSolutionSet :: (Ord var, Show var, Eq coeff) => Expected (Outputs var coeff) -> [Solution var coeff] -> TestResult
analyzeSolutionSet mbExpected sols = 
  case results of
    [] -> case mbExpected of
            ShouldFail -> Passed             -- no solutions, but we didn't expect any, so this is right!
            _          -> Failed             -- no solutions, but we expected some
    _  -> if any wasUnsolved results
            then Unsolved                 -- we couldn't solve all branches
            else if any didFail results
              then Failed                 -- at least one solution gives the wrong output
              else if all didPass results
                then Passed               -- everything is fine
                else error "analyzeSolutions: fatal: should not happen"
  where
    results = case mbExpected of
      Expecting expected -> map (analyzeSingleSolution1 expected    ) sols
      OneOf expected_set -> map (analyzeSingleSolution2 expected_set) sols
      ShouldFail         -> map (analyzeSingleSolution1 emptyOutputs) sols
      DontCare           -> map (analyzeSingleSolution1 emptyOutputs) sols

----------------------------------------
-- this stuff is needed only to support the 'OneOf' feature

-- | For each element of the acceptable set, we restrict the 
-- mapping to those variables, and accept if matches
checkIfMappingIsAcceptable :: (Ord var, Show var, Eq coeff) => Mapping var coeff -> [Outputs var coeff] -> TestResult
checkIfMappingIsAcceptable mapping = go where
  go []              = Failed
  go (expected:rest) = let expected_mapping = fromOutputs expected       in
                       let outVars = extractMappingKeys expected_mapping in
                       if restrictMapping outVars mapping == expected_mapping
                          then Passed
                          else go rest

checkIfSubstitutionIsAcceptable :: (Ord var, Show var, Eq coeff) => Substitution var coeff -> [Outputs var coeff] -> TestResult
checkIfSubstitutionIsAcceptable (Substitution subs) = go where
  go []              = Failed
  go (expected:rest) =
    let expected_mapping = fromOutputs expected            in
    let outVars = extractMappingKeys expected_mapping      in
    let candidate = restrictMapping outVars (Mapping subs) in
    let assocs    = Map.assocs (fromMapping candidate)     in
    if all rhsIsValue (map snd assocs)
        then let mapping = Mapping $ Map.fromList [ (key,rhs) | (key,mbrhs) <- assocs , let Just rhs = rhsMbValue mbrhs ] in
             if restrictMapping outVars mapping == expected_mapping
               then Passed 
               else Failed
        else Failed

-- /// a set of possible expected values version ///
analyzeSingleSolution2 :: forall var coeff. (Ord var, Show var, Eq coeff) => [Outputs var coeff] -> Solution var coeff -> TestResult
analyzeSingleSolution2 expected_set sol = 

  case sol of
    UniqueSolution   table      -> checkIfMappingIsAcceptable (Mapping table) expected_set
    HasFreeVariables free subs  -> checkIfSubstitutionIsAcceptable subs       expected_set   
    CouldntSolve     _          -> Unsolved

----------------------------------------

-- /// a definite expected value version ///
analyzeSingleSolution1 :: forall var coeff. (Ord var, Show var, Eq coeff) => Outputs var coeff -> Solution var coeff -> TestResult
analyzeSingleSolution1 (Outputs expected) sol = 

  case sol of
    UniqueSolution   table      -> checkMapping (Mapping table)
    HasFreeVariables free subs  -> checkSubstitution subs
    CouldntSolve     _          -> Unsolved

  where

    outVars = extractMappingKeys expected

    checkMapping :: Mapping var coeff -> TestResult
    checkMapping mapping = 
      if restrictMapping outVars mapping == expected
        then Passed 
        else Failed

    checkSubstitution :: Substitution var coeff -> TestResult
    checkSubstitution (Substitution subs) = 
      let candidate = restrictMapping outVars (Mapping subs) in
      let assocs    = Map.assocs (fromMapping candidate)     in
      if all rhsIsValue (map snd assocs)
          then let mapping = Mapping $ Map.fromList [ (key,rhs) | (key,mbrhs) <- assocs , let Just rhs = rhsMbValue mbrhs ] in
               if mapping == expected
                 then Passed
                 else Failed
          else Failed

--------------------------------------------------------------------------------

type MkInOutPair testcase 
  = (testcase -> 
      (           Inputs  Name Integer
      , Expected (Outputs Name Integer)
      ))

runSemanticTests
  :: Show testcase 
  => Verbosity 
  -> SymConstraints Field 
  -> MkInOutPair testcase
  -> [testcase] 
  -> IO Bool
runSemanticTests verbosity symConstr makeInputOutputPair testcases = do

  let ntests = length testcases

  results <- forM (zip [1..] testcases) $ \(i,testcase) -> do
    when (verbosity >= Info) $ putStr $ "running test #" ++ show i ++ "... "
    let (inputs,expected_) = makeInputOutputPair testcase
    let expected = fmap (mapCoeff fromInteger) expected_
    let ini = fromMapping (fromInputs inputs)
    sols <- solver verbosity symConstr (Map.map fromInteger ini)
    let n = length sols
    when (verbosity >= Info) $ when (n > 1) $ putStr ("warning: multiple witness solutions! n = " ++ show n ++ "... ")
    let res = analyzeSolutionSet expected sols
    when (verbosity >= Info) $ case (sols, expected) of
      ([] , ShouldFail) -> putStrLn "Passed (no solutions are found, but none is expected)"
      ([] , _         ) -> putStrLn "Failed (there are no solutions, but some solution(s) are expected)"
      _                 -> print res
    return res
          
  putStrLn $ "=================="
  putStrLn $ "from a total of " ++ show ntests ++ " test cases:"
  putStrLn $ "- passed:   " ++ show (count didPass     results)
  putStrLn $ "- failed:   " ++ show (count didFail     results)
  putStrLn $ "- unsolved: " ++ show (count wasUnsolved results)

  return (all didPass results)

--------------------------------------------------------------------------------

runExample :: Verbosity -> CircuitFiles -> Inputs Name Integer -> IO [Solution Name Field]
runExample verbosity circuitFiles inputs = do
  symConstr <- loadCircuit verbosity circuitFiles 
  let ini = fromMapping (fromInputs inputs)
  sols <- solver verbosity symConstr (Map.map fromInteger ini)
  return sols

--------------------------------------------------------------------------------

loadCircuit :: Verbosity -> CircuitFiles -> IO (SymConstraints Field)
loadCircuit verbosity circuitFiles = do

  constr0 <- parseJsonFile (_jsonFile circuitFiles)
  sym0    <- parseSymFile  (_symFile  circuitFiles) 

  let constr = recognizeRationals constr0
  let sym    = symbolTableMapName dropMain sym0

  when (verbosity >= Debug) $ do
    printSymbolTable sym
    printConstraints (symbolicRatPrinter sym) constr

  let constr65537 = replacePrime 65537 $ constraintsMapCoeff toField constr
  let symConstr   = SymConstraints constr65537 sym

  return symConstr

--------------------------------------------------------------------------------

loadWitness :: Verbosity -> CircuitFiles -> WitnessFiles -> IO (Witness Name Field)
loadWitness verbosity circuitfiles witnessfiles = do
  sym0  <- parseSymFile  (_symFile circuitfiles) 
  mapCoeff toField_ <$> loadWitness' sym0 witnessfiles

loadWitness' :: SymbolTable -> WitnessFiles -> IO (Witness Name Integer)
loadWitness' symboltable witnessfiles = do
  values <- parseWtnsJsonFile (_wtnsJsonFile witnessfiles)
  return $ mkWitness symboltable values

--------------------------------------------------------------------------------

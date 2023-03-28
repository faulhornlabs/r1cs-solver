
-- | Specification of a test
--
-- Most tests follow the same pattern, which we factor out here
--
-- However, sometimes we need more customized solutions, 
-- so using approach is not mandatory
--

{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE RecordWildCards #-}
module R1CS.Test.Spec
  ( TestSpec(TestSpec)
  , testSemantics
  , testSemanticsMany
  ) 
  where

--------------------------------------------------------------------------------

import Control.Monad

import R1CS.Witness
import R1CS.Compile
import R1CS.Test.Runner
import R1CS.Misc

--------------------------------------------------------------------------------

-- | The specification of a test. Haddock is broken, so I just copy-paste the definition here:
--
-- > data TestSpec testcase output = TestSpec
-- >   { circomFile    :: FilePath                             -- ^ the source @.circom@ file                             
-- >   , mainComponent :: MainComponent                        -- ^ the main component 
-- >   , inputs        :: (testcase -> Inputs  Name Integer)   -- ^ the inputs of a given test case
-- >   , outputs       :: (output   -> Outputs Name Integer)   -- ^ the output mapping
-- >   , semantics     :: (testcase -> Expected output)        -- ^ the expected semantics
-- >   , testCases     :: [testcase]                           -- ^ list of test cases
-- >   }
--
data TestSpec testcase output = TestSpec
  { circomFile    :: !FilePath                                -- ^ the source @.circom@ file                             
  , mainComponent :: !MainComponent                           -- ^ the main component 
  , inputs        :: !(testcase -> Inputs  Name Integer)      -- ^ the inputs of a given test case
  , outputs       :: !(output   -> Outputs Name Integer)      -- ^ the output mapping
  , semantics     :: !(testcase -> Expected output)           -- ^ the expected semantics
  , testCases     :: ![testcase]                              -- ^ list of test cases
  }

-- note: making these fields strict makes GHC throw an error if we accidentally miss some

--------------------------------------------------------------------------------

-- | Run the semantics tests for a given specifications
testSemantics :: Show testcase => TestSpec testcase output -> Verbosity -> IO ()
testSemantics (TestSpec{..}) verbosity = do
  putStrLn $ "\ntesting template `" ++ (_templateName mainComponent) ++ "` from `" ++ circomFile ++ "`"
  -- compile the circuit using circom
  circuitFiles <- compileCircomCircuit verbosity circomFile (Just mainComponent)
  -- load the JSON + SYM files
  circuit <- loadCircuit verbosity circuitFiles
  -- run the tests
  let mkInOutPair = \testcase -> (inputs testcase, fmap outputs (semantics testcase))
  ok <- runSemanticTests verbosity circuit mkInOutPair testCases
  putStrLn $ if ok then "OK" else "PROBLEMS"

--------------------------------------------------------------------------------

-- | Run the semantics tests for multiple specifications. This is useful for example
-- when you have a template with parameters, and you want to test for multiple parameter settings.
--
testSemanticsMany :: (Show gp, Show testcase) => [ (gp , TestSpec testcase output) ] -> Verbosity -> IO ()
testSemanticsMany specList verbosity = do
  forM_ specList $ \(gp, TestSpec{..}) -> do
    putStrLn $ "\ntesting template `" ++ (_templateName mainComponent) ++ "` from `" ++ circomFile ++ "` " ++
               "with global parameter = " ++ show gp
    -- (re)compile the circuit using circom with the given global parameters
    circuitFiles <- compileCircomCircuit verbosity circomFile (Just mainComponent)
    -- load the JSON + SYM files
    circuit <- loadCircuit verbosity circuitFiles
    -- run the tests
    let mkInOutPair = \testcase -> (inputs testcase, fmap outputs (semantics testcase))
    ok <- runSemanticTests verbosity circuit mkInOutPair testCases
    putStrLn $ if ok then "OK" else "PROBLEMS"

--------------------------------------------------------------------------------

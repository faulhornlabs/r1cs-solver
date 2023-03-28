
-- | Testing some simple example circuits
--

module Simple.Main where

--------------------------------------------------------------------------------

import R1CS.Misc ( Verbosity(..) )

import qualified R1CS.Test.Spec as Spec

import qualified Simple.Misc.IsBoolean   as IsBoolean               
import qualified Simple.Misc.SumOfCubes  as SumOfCubes
import qualified Simple.Misc.StdBasis    as StdBasis
import qualified Simple.Misc.Fibonacci   as Fibonacci
import qualified Simple.Misc.RightShift  as RightShift

--------------------------------------------------------------------------------

testSimple :: IO ()
testSimple = testSimple' Silent

testSimple' :: Verbosity -> IO ()
testSimple' verbosity = do

  let runSpec     what = Spec.testSemantics     what verbosity
  let runSpecMany what = Spec.testSemanticsMany what verbosity

  runSpec     IsBoolean.spec                     
  runSpec     SumOfCubes.spec       
  runSpec     StdBasis.spec
  runSpecMany Fibonacci.specs

  -- the unsound example:
  Spec.testSemantics RightShift.spec (max Info verbosity)
  RightShift.testHonestProver verbosity

--------------------------------------------------------------------------------


-- | This module re-exports all kind of stuff for convenience, 
-- so that you can do testing of circuits by importing only this one.
-- 

module R1CS
  ( module Data.Map
  , module R1CS.Witness
  , module R1CS.Constraints
  , module R1CS.Solver
  , module R1CS.Test.Spec
  , module R1CS.Test.Runner
  , module R1CS.Compile
  , module R1CS.Misc      
  )
  where

--------------------------------------------------------------------------------

import Data.Map (Map) ; import qualified Data.Map.Strict as Map

import R1CS.Constraints

import R1CS.Solver 
  ( solver 
  , Substitution(..) , printSubstitution 
  , RHS(..) , rhsIsValue , rhsMbValue
  , Solution(..) , Solution , printSolution , printUniqueSolution
  , Partial(..)
  )

import R1CS.Witness
import R1CS.Test.Spec
import R1CS.Test.Runner
import R1CS.Compile
import R1CS.Misc ( Verbosity(..) , Prime , Name , Bit(..) )

--------------------------------------------------------------------------------

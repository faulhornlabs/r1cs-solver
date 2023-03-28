
module Simple.Common
  ( module R1CS
  , module System.FilePath
  , circuitSourceDir
  )
  where 

--------------------------------------------------------------------------------

import System.FilePath
import R1CS

--------------------------------------------------------------------------------

circuitSourceDir :: FilePath
circuitSourceDir = "examples/circuits/simple"

--------------------------------------------------------------------------------

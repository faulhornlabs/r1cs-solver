
-- | Handles the external parts of the toolchain: @circom@, @snarkjs@ etc

module R1CS.Compile where

--------------------------------------------------------------------------------

import Data.Char
import Data.List

import Control.Monad
import Control.Exception

import System.IO
import System.Process
import System.Directory
import System.FilePath
import System.Exit
import System.Random

import qualified Data.Aeson as J

import R1CS.Misc
import R1CS.Witness

--------------------------------------------------------------------------------

-- | Location of the @circom@ and @snarkjs@ executables
data ExtConfig = ExtConfig
  { _circomExe  :: FilePath
  , _snarkjsExe :: FilePath
  , _nodejsExe  :: FilePath
  }
  deriving Show

defaultExtConfig :: ExtConfig
defaultExtConfig = ExtConfig 
  { _circomExe  = "circom"
  , _snarkjsExe = "snarkjs"
  , _nodejsExe  = "node"
  }

--------------------------------------------------------------------------------

-- | The files encoding an R1CS circuit
data CircuitFiles = CircuitFiles 
  { _r1csFile :: FilePath     -- ^ the @.r1cs@ output of circom
  , _symFile  :: FilePath     -- ^ the @.sym@ output of circom
  , _jsonFile :: FilePath     -- ^ the R1CS converted to @.json@ by snarkjs
  , _wasmDir  :: FilePath     -- ^ the directory for WASM witness computation code
  , _wasmFile :: FilePath     -- ^ the WASM file (inside the WASM directory)
  }
  deriving Show

-- mapCircuitFiles :: (FilePath -> FilePath) -> CircuitFiles -> CircuitFiles
-- mapCircuitFiles f (CircuitFiles x y z u v) = CircuitFiles (f x) (f y) (f z) (f u) (f v)

--------------------------------------------------------------------------------

data WitnessFiles = WitnessFiles
  { _inputFile    :: FilePath     -- ^ @input.json@
  , _wtnsFile     :: FilePath     -- ^ @witness.wtns@
  , _wtnsJsonFile :: FilePath     -- ^ @witness.json@ (converted by snarkjs)
  }
  deriving Show

data MainComponent = MainComponent 
  { _templateName   :: String
  , _templateParams :: [Int]
  , _publicInputs   :: [Name] 
  }
  deriving Show

mainComponentLine :: MainComponent -> String
mainComponentLine (MainComponent template params publics)  
  =  "component main { public [ " ++ (intercalate ", " publics) ++ " ] } = "
  ++ template ++ "(" ++ intercalate ", " (map show params) ++ ");"

--------------------------------------------------------------------------------
-- * compiling circuits

quote :: String -> String
quote fn = "\"" ++ fn ++ "\""

-- | Compile a circom circuit. It will put the build artifacts
-- into a @build@ subdirectory.
compileCircomCircuit :: Verbosity -> FilePath -> Maybe MainComponent -> IO CircuitFiles
compileCircomCircuit = compileCircomCircuit' defaultExtConfig 

compileCircomCircuit' :: ExtConfig -> Verbosity -> FilePath -> Maybe MainComponent -> IO CircuitFiles
compileCircomCircuit' extcfg verbosity circomFile0 mbMain = case mbMain of

  Nothing -> do
    circomFile <- (expandTilde circomFile0 >>= canonicalizePath)
    putStrLn $ "compiling " ++ circomFile
    compileCircomCircuit'' extcfg verbosity circomFile

  -- we create a temporary file with the main component, compile that, 
  -- then delete the temp file and rename all the outputs...
  -- /// if only circom would support standard input like every normal tool... ///
  Just mainComp -> do

    -- normalize input file path
    circomFile <- (expandTilde circomFile0 >>= canonicalizePath)
    putStrLn $ "compiling " ++ circomFile

    -- add main component
    text <- readFile circomFile
    let text' = text ++ "\n" ++ mainComponentLine mainComp

    -- write this into a temporary file
    uid <- randomUID
    let tmpCircomFile = mangleFileName uid circomFile
    writeFile tmpCircomFile text'

    -- compile
    circuitfiles <- withFinalizer (removeFile tmpCircomFile) $ do
      compileCircomCircuit'' extcfg verbosity tmpCircomFile

    -- delete the temp file and rename everything back to normal
    removeFile tmpCircomFile
    renameDemangleCircuitFiles uid circuitfiles

compileCircomCircuit'' :: ExtConfig -> Verbosity -> FilePath -> IO CircuitFiles
compileCircomCircuit'' extcfg verbosity circomFile0 = do
 
  circomFile <- (expandTilde circomFile0 >>= canonicalizePath)
  let baseName = dropExtension (takeFileName circomFile) 
  let ExtConfig circomExe snarkjsExe nodejsExe = extcfg
  circomFile <- (expandTilde circomFile0 >>= canonicalizePath)
 
  let srcDir   = dropFileName circomFile
  let tgtDir   = srcDir </> "build"
  createDirectoryIfMissing False tgtDir
  let r1csFile = tgtDir </> addExtension baseName "r1cs"
  let symFile  = tgtDir </> addExtension baseName "sym"
  let jsonFile = tgtDir </> addExtension baseName "json"
  let wasmDir  = tgtDir </> (baseName ++ "_js")
  let wasmFile = tgtDir </> (baseName ++ "_js") </> addExtension baseName "wasm"
 
{-
  -- safer but does not support >/dev/null
  exit1 <- rawSystem circomExe $
    [ "--r1cs" 
    , "--sym"  
    , "--wasm" 
    , "--O2" 
    , "--output"
    , tgtDir
    , circomFile
    ] 
-}
  let devnull = if (verbosity <= Info) then " >/dev/null" else ""
  let cmd1 = circomExe ++ " --r1cs --sym --wasm --O2 --output " ++ quote tgtDir ++ " " ++ quote circomFile ++ devnull
  when (verbosity >= Verbose) $ putStrLn cmd1
  exit1 <- system cmd1
  unless (exit1 == ExitSuccess) $ fail $ "circom exited with " ++ show exit1

{-
  -- safer but does not support >/dev/null
  exit2 <- rawSystem snarkjsExe $
    [ "rej"
    , r1csFile
    , jsonFile
    ] 
-}
  let cmd2 = snarkjsExe ++ " rej " ++ quote r1csFile ++ " " ++ quote jsonFile ++ devnull
  when (verbosity >= Verbose) $ putStrLn cmd2
  exit2 <- system cmd2
  unless (exit2 == ExitSuccess) $ fail $ "snarkjs exited with " ++ show exit2
 
  return $ CircuitFiles 
    { _r1csFile = r1csFile
    , _symFile  = symFile
    , _jsonFile = jsonFile
    , _wasmDir  = wasmDir
    , _wasmFile = wasmFile
    }

--------------------------------------------------------------------------------
-- * generating witness

computeWitness :: Verbosity -> CircuitFiles -> HsInputs -> IO WitnessFiles 
computeWitness verbosity circuitfiles hsinputs = do
  let baseName = takeBaseName (_r1csFile circuitfiles)
  tmpDir <- getTemporaryDirectory
  uid    <- randomUID
  let witnessfiles = WitnessFiles 
        { _inputFile    = tmpDir </> (baseName ++ "_input_"   ++ uid) <.> ".json"
        , _wtnsFile     = tmpDir </> (baseName ++ "_witness_" ++ uid) <.> ".wtns"
        , _wtnsJsonFile = tmpDir </> (baseName ++ "_witness_" ++ uid) <.> ".json"
        }   
  let jsoninput = hsInputsToJSON hsinputs
  J.encodeFile (_inputFile witnessfiles) jsoninput 
  computeWitness' defaultExtConfig verbosity circuitfiles witnessfiles 
  return witnessfiles

computeWitness' :: ExtConfig -> Verbosity -> CircuitFiles -> WitnessFiles -> IO ()
computeWitness' extcfg verbosity circuitfiles witnessfiles = do

  let devnull = if (verbosity <= Info) then " >/dev/null" else ""

  -- node generate_witness.js multiplier2.wasm input.json witness.wtns
  let cmd1 =  (_nodejsExe extcfg) ++ " " 
           ++ quote (_wasmDir   circuitfiles </> "generate_witness.js") ++ " "
           ++ quote (_wasmFile  circuitfiles) ++ " "
           ++ quote (_inputFile witnessfiles) ++ " "
           ++ quote (_wtnsFile  witnessfiles) ++ devnull
  when (verbosity >= Verbose) $ putStrLn cmd1
  exit1 <- system cmd1
  unless (exit1 == ExitSuccess) $ fail $ "node generate_witness.js exited with " ++ show exit1

  -- wtns export json              Calculate the witness with debug info.
  let cmd2 =  (_snarkjsExe extcfg) ++ " wej " 
           ++ quote (_wtnsFile      witnessfiles) ++ " "
           ++ quote (_wtnsJsonFile  witnessfiles) ++ devnull
  when (verbosity >= Verbose) $ putStrLn cmd2
  exit2 <- system cmd2
  unless (exit2 == ExitSuccess) $ fail $ "snarkjs wej exited with " ++ show exit2

--------------------------------------------------------------------------------
-- * Filepath mangling

-- | The @~/...@ in unix paths is supposed to be handled by the shell. 
-- But we also do it for convenience of experimenting inside GHCi
expandTilde :: FilePath -> IO FilePath
expandTilde p =
  case dropWhile isSpace p of
    ('~':d) -> do
      tilde <- getHomeDirectory   -- will fail if HOME not defined
      return (tilde ++ '/':d)
    other -> return other

--------------------------------------------------------------------------------
-- ** lot of hacks just to work around circom command line limitations...

type UID = String

-- | Replaces @"/foo/bar/alma.txt"@ with @"/foo/bar/alma_12345.txt"@
mangleFileName :: UID -> FilePath -> FilePath 
mangleFileName uid fpath = fpath' where
  (rest,origext) = splitExtension fpath
  fpath' = (rest ++ "_" ++ uid) <.> origext

-- | Reverses the above mangling
demangleFileName :: UID -> FilePath -> FilePath
demangleFileName uid fpath = fpath' where
  (tmp ,origext) = splitExtension fpath
  fpath' = case dropFromEnd "js" tmp of
    Nothing -> case dropFromEnd uid tmp of
      Nothing   -> error "demangleFileName: the given filename does not have expected format!"
      Just rest -> rest <.> origext
    Just tmp2 -> if origext /= ""
      then error "demangleFileName: expecting a diretory ending with \"_js\""
      else case dropFromEnd uid tmp2 of
        Nothing   -> error "demangleFileName: the given filename does not have expected format!"
        Just rest -> rest ++ "_js"

renameDemangleCircuitFiles :: UID -> CircuitFiles -> IO CircuitFiles
renameDemangleCircuitFiles uid mangled = do
  r1csFile' <- renameMangledFile uid (_r1csFile mangled)
  jsonFile' <- renameMangledFile uid (_jsonFile mangled)
  symFile'  <- renameMangledFile uid (_symFile  mangled)

  -- first the WASM file in the subdirectory...
  _wasmFile <- renameMangledFile uid (_wasmFile mangled)      

  -- if the target "_js" directory exists (say because an earlier compilation)
  -- then we have to remove it first...
  let wasmDir' = demangleFileName uid (_wasmDir mangled)
  doesDirectoryExist wasmDir' >>= \b -> when b (removeDirectoryRecursive wasmDir')

  -- and *now* we can rename the directory
  wasmDir'  <- renameMangledFile uid (_wasmDir  mangled)    

  let baseName  = dropExtension (takeFileName r1csFile')
  let baseDir   = dropFileName r1csFile'
  let wasmFile' = baseDir </> (baseName ++ "_js") </> (baseName <.> "wasm")
  return $ CircuitFiles 
    { _r1csFile = r1csFile'
    , _symFile  = symFile'
    , _jsonFile = jsonFile'
    , _wasmDir  = wasmDir'
    , _wasmFile = wasmFile'
    }

renameMangledFile :: UID -> FilePath -> IO FilePath
renameMangledFile uid fpath = do
  let demangled = demangleFileName uid fpath
  renamePath fpath demangled
  return demangled

-- | Drops the @"_12345"@ from the end
dropFromEnd :: String -> String -> Maybe String
dropFromEnd what text = text' where
  len   = length what
  rev   = reverse text
  text' = if take (len+1) rev == reverse ('_':what)
            then Just $ reverse $ drop (len+1) rev
            else Nothing

randomUID :: IO String
randomUID = replicateM 12 rndChar where
  rndChar = f <$> randomRIO (0, 26+26+10-1)
  f :: Int -> Char
  f k | k < 10    = chr (48+k   )
      | k < 36    = chr (65+k-10)
      | k < 62    = chr (97+k-36)
      | otherwise = error "randomUID: fatal: should not happen"

--------------------------------------------------------------------------------
-- ** exception handling 

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

withFinalizer :: IO () -> IO a -> IO a
withFinalizer finalizer action = catchIO action handler where
  handler exc = do
    finalizer
    throwIO exc      -- rethrow

--------------------------------------------------------------------------------

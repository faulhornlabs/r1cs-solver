
-- | Parse the @.sym@ output of @circom@

{-# LANGUAGE TypeApplications #-}
module R1CS.Parser.SymFile where

--------------------------------------------------------------------------------

import Data.List
import Data.List.Split

import Control.Monad
import Text.Read

import Data.Map (Map)
import qualified Data.Map as Map

import R1CS.Constraints
import R1CS.Misc

--------------------------------------------------------------------------------

parseSymFile :: FilePath -> IO SymbolTable
parseSymFile fname = do
  text <- readFile fname
  return $ parseSymbolTable fname text

--------------------------------------------------------------------------------

data SourceLine = SourceLine  
  { _fileName :: !FilePath
  , _lineNo   :: !Int
  }
  deriving (Eq,Ord,Show)

parseError :: SourceLine -> String -> a 
parseError (SourceLine fn line) msg = 
  error $ "parse error in line " ++ show line ++ " of file `" ++ fn ++ "`:\n  " ++ msg

parseSymLine :: SourceLine -> String -> (Name,Location)
parseSymLine srcloc text = 
  case splitOn "," text of
    [aa,bb,cc,nn] -> case (readMaybe @Int aa, readMaybe @Int bb, readMaybe @Int cc) of
      (Nothing,_,_) -> err "cannot parse variable index"
      (_,Nothing,_) -> err "cannot parse variable location"
      (_,_,Nothing) -> err "cannot parse `0`"
      (Just a, Just b, Just c) -> 
        let idx = if b >= 0 then Just b else Nothing
        in  (nn, Location a idx c)
    _ -> err "expecting 4 comma-separated fields"
  where
    err = parseError srcloc

parseSymbolTable_ :: FilePath -> String -> [(Name,Location)]
parseSymbolTable_ fname text = zipWith parseSymLine srclocs (lines text) where
  srclocs = [ SourceLine fname i | i<-[1..] ]

parseSymbolTable :: FilePath -> String -> SymbolTable
parseSymbolTable fname text = symboltable where
  list = parseSymbolTable_ fname text
  symboltable = SymbolTable symlkp varlkp
  symlkp = Map.fromList list
  varlkp = Map.fromList [ (idx,(name,loc)) | (name, loc@(Location orig (Just idx) _)) <- list ]

--------------------------------------------------------------------------------

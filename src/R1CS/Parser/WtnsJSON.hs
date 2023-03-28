
-- | Parse the JSON witness exported @snarkjs wej@
--
-- This is simply a list of strings containing field elements.
-- The @.sym@ file contains the mapping.
--
{-# LANGUAGE TypeApplications #-}
module R1CS.Parser.WtnsJSON where

--------------------------------------------------------------------------------

import Data.List
import Data.Ratio
import Data.Maybe

import Text.Read ( readMaybe )

import Control.Monad
import System.IO

-- i hate aeson

import qualified Data.Foldable   as F
import qualified Data.ByteString as B

import Data.Text           (Text)    ; import qualified Data.Text           as T
import Data.Vector         (Vector)  ; import qualified Data.Vector         as V

import Data.Aeson.Types ( Value(..) , Array )
import Data.Aeson       ( eitherDecodeStrict )

import R1CS.Constraints

--------------------------------------------------------------------------------

type WitnessValues = [Integer]

parseWtnsJsonFile :: FilePath -> IO WitnessValues
parseWtnsJsonFile fname = do
  json <- loadJSON fname
  return $ parseJsonValue json

loadJSON :: FilePath -> IO Value
loadJSON fname = do
  bs <- B.readFile fname
  case eitherDecodeStrict bs of
    Left  msg  -> error $ "parse error while decoding JSON file `" ++ fname ++ "`:\n  " ++ msg
    Right json -> return json

--------------------------------------------------------------------------------


valueToInteger :: Value -> Integer
valueToInteger v = case v of
  String t -> case readMaybe (T.unpack t) of 
    Nothing  -> error $ "expecting a JSON string containing an integer"
    Just y   -> y 
  _        -> error $ "expecting a string JSON value"

--------------------------------------------------------------------------------

parseJsonValue :: Value -> WitnessValues
parseJsonValue json = case json of
  Array arr -> map valueToInteger $ V.toList arr
  _         -> error $ "expecting a JSON array"

--------------------------------------------------------------------------------

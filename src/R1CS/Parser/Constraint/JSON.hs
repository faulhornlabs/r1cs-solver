
-- | Parse the JSON contraints output by @snarkjs rej@

{-# LANGUAGE TypeApplications #-}
module R1CS.Parser.Constraint.JSON where

--------------------------------------------------------------------------------

import Data.List
import Data.Ratio
import Data.Maybe


import Control.Monad
import System.IO

-- i hate aeson

import qualified Data.Foldable   as F
import qualified Data.ByteString as B
import qualified Data.Scientific as S

import Data.Set            (Set)     ; import qualified Data.Set            as Set
import Data.Map            (Map)     ; import qualified Data.Map.Strict     as Map
import Data.Text           (Text)    ; import qualified Data.Text           as T
import Data.Vector         (Vector)  ; import qualified Data.Vector         as V
import Data.Aeson.KeyMap   (KeyMap)  ; import qualified Data.Aeson.KeyMap   as KMap
import Data.Aeson.Key      (Key)     ; import qualified Data.Aeson.Key      as K

import Data.Aeson.Types ( Value(..) , Object , Array )
import Data.Aeson       ( eitherDecodeStrict )

import R1CS.Constraints

--------------------------------------------------------------------------------

parseJsonFile :: FilePath -> IO (Constraints Int Integer)
parseJsonFile fname = do
  json <- loadJSON fname
  return $ parseJsonValue json

loadJSON :: FilePath -> IO Value
loadJSON fname = do
  bs <- B.readFile fname
  case eitherDecodeStrict bs of
    Left  msg  -> error $ "parse error while decoding JSON file `" ++ fname ++ "`:\n  " ++ msg
    Right json -> return json

--------------------------------------------------------------------------------
-- i hate Aeson

getField :: String -> Value -> Value
getField key json = case json of 
  Object hmap -> case KMap.lookup (K.fromString key) hmap of
    Nothing     -> error $ "field `" ++ key ++ "` not found in JSON object" 
    Just val    -> val
  _ -> error $ "expecting a JSON object"

getStringField :: String -> Value -> String
getStringField key json = case getField key json of
  String t -> T.unpack t
  _        -> error $ "expecting JSON field `" ++ key ++ "` to have type String"

getBoolField :: String -> Value -> Bool
getBoolField key json = case getField key json of
  Bool b -> b
  _      -> error $ "expecting JSON field `" ++ key ++ "` to have type Bool"

getIntField :: String -> Value -> Int
getIntField key json = case getField key json of
  Number n -> case S.toBoundedInteger n of
    Just i   -> i
    Nothing  -> error $ "JSON field `" ++ key ++ "` is not an integer"
  _        -> error $ "expecting JSON field `" ++ key ++ "` to have type Int"

-- type Array = Vector Value
getArrayField :: String -> Value -> Array
getArrayField key json = case getField key json of
  Array a -> a
  _       -> error $ "expecting JSON field `" ++ key ++ "` to have type Array"

getListField :: String -> Value -> [Value]
getListField key json = F.toList (getArrayField key json)

-- type Object = KeyMap Value
getObjectField :: String -> Value -> Object
getObjectField key json = case getField key json of
  Object o -> o
  _        -> error $ "expecting JSON field `" ++ key ++ "` to have type Object"

valueToInt :: Value -> Int
valueToInt v = case v of
  Number n -> case S.toBoundedInteger n of
    Just i   -> i
    Nothing  -> error $ "expecting an integral JSON value"
  _ -> error $ "expecting a numeric JSON value"

valueToString :: Value -> String
valueToString v = case v of
  String t -> T.unpack t
  _        -> error $ "expecting a string JSON value"

--------------------------------------------------------------------------------

parseLinComb :: Value -> LinComb Int Integer
parseLinComb v = 
  case v of
    Object o -> LinComb $ Map.fromList $ map f $ KMap.toList o
    _        -> error "expecting a JSON object, representing a linear combination"
  where 
    f (key,v) = ( read @Int     $ K.toString    key 
                , read @Integer $ valueToString v
                )

parseSingleConstraint :: Value -> R1CS Int Integer
parseSingleConstraint v = case v of
  Array a -> case F.toList a of
    [v1,v2,v3] -> R1CS (parseLinComb v1)
                       (parseLinComb v2)
                       (parseLinComb v3)
    _ -> error "expecting a JSON array of size 3, representing an R1CS constraint"
  _ -> error "expecting a JSON array of size 3, representing an R1CS constraint"

parseJsonValue :: Value -> (Constraints Int Integer)
parseJsonValue json = 

  case getBoolField "useCustomGates" json of
    False -> final
    True  -> error "custom gates are not supported" 

  where
    final   = Constraints prime r1cs_list where

    prime   = read @Integer  $ getStringField "prime"       json
    nvars   = read @Int      $ getStringField "nVars"       json      -- number of witness variables
    nlabels = read @Int      $ getStringField "nLabels"     json      -- number of variable labels (names)
    constr  =                  getListField   "constraints" json
    mapping = map valueToInt $ getListField   "map"         json

    r1cs_list = map parseSingleConstraint constr

--------------------------------------------------------------------------------


-- | Parse the constraints from the output of @snarkjs rp@
--
-- NOTE: while this parses just fine, the output from @snarkjs rp@
-- we try to parse is nonsensically buggy:
--
-- see <https://github.com/iden3/snarkjs/issues/324>

{-# LANGUAGE PackageImports #-}
module R1CS.Parser.Constraint.Text where

--------------------------------------------------------------------------------

import Data.List
import Data.Ratio
import Data.Maybe

import Control.Monad
import System.IO

import Data.Set (Set) ; import qualified Data.Set as Set
import Data.Map (Map) ; import qualified Data.Map as Map

import "parsec1" Text.ParserCombinators.Parsec

import R1CS.Constraints
import R1CS.Params

--------------------------------------------------------------------------------

identP :: Parser String
identP = do
  x  <- letter
  xs <- many (alphaNum <|> oneOf "_.[]")
  return (x:xs)

numberP :: Parser Integer
numberP = do
  ds <- many1 digit
  return (read ds)

linearP :: Parser (LinComb String Integer)
linearP = do  
  char '[' ; spaces
  ls <- linearTermP `sepBy` (char '+' >> spaces)
  char ']' ; spaces
  return $ LinComb $ Map.fromList ls

linearTermP :: Parser (String,Integer)
linearTermP = 
  do
    stuff <- try pair <|> try ident <|> num -- try one <|> zero
    spaces
    return stuff
  where
    pair  = do { k <- numberP   ; spaces ; name <- identP 
                                ; return (name, k) }
    ident = do { name <- identP ; return (name, 1) }
    num   = do { k <- numberP   ; return ("1",k) }
    -- zero = return ("1",0)

lineP :: Parser (R1CS String Integer)
lineP = do
  a <- linearP
  char '*' ; spaces
  b <- linearP
  char '-' ; spaces
  c <- linearP
  char '=' ; spaces
  char '0' ; spaces
  eof
  return (R1CS a b c)

parseLine :: Int -> String -> R1CS String Integer
parseLine i str = case parse lineP ("<line #" ++ show i ++ ">") str of
  Right x   -> x
  Left  msg -> error (show msg)

parseTextFile :: FilePath -> IO (Constraints String Integer)
parseTextFile fname = do
  text <- readFile fname
  let r1cs_list = zipWith parseLine [1..] (lines text)
  return $ Constraints bn128_prime r1cs_list

--------------------------------------------------------------------------------


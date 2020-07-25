-- Note: I do not know how to test the functions in the test folder without exporting them
-- A compilation flag to export private functions only when testing them could be a get around
module Lib
    ( countUniqueAndNonUnique
    , cuanuNaive
    , digits
    , toLuhnForm
    , isLuhn
    , sirenFromInt
    , sirenTextToList
    ) where

import Data.List
import Data.Maybe
import Text.Read
import Data.Map (Map)
import qualified Data.Map as Map

newtype Siren = Siren Int 
    deriving (Ord, Eq, Show)

sirenTextToList :: String -> [Siren]
sirenTextToList = mapMaybe ((>>= sirenFromInt) . readMaybe) . lines

sirenFromInt :: Integral a => a -> Maybe Siren
sirenFromInt x 
    | 0 <= x && x < 10^9 && isLuhn x = Just (Siren (fromIntegral x))
    | otherwise = Nothing

isLuhn :: Integral a => a -> Bool
isLuhn x = mod (sum $ toLuhnForm x) 10 == 0

toLuhnForm :: Integral a => a -> [a]
toLuhnForm x = helper (reverse $ digits x) []
    where
        helper (a:b:xs) acc = helper xs (sumDigit (2*b):a:acc)
        helper [x] acc = x:acc
        helper [] acc = acc
        sumDigit x = if x > 9 then x-9 else x 

digits :: Integral a => a -> [a]
digits 0 = [0]
digits x = helper x []
    where
        helper 0 acc = acc
        helper x acc = helper q (r:acc)
            where (q, r) = quotRem x 10

countUniqueAndNonUnique :: (Ord k, Integral a1, Integral a2) => [k] -> (a1, a2)
countUniqueAndNonUnique l = helper l Map.empty 0 0
  where 
    helper [] _ lengthUnique lengthNonUnique = (lengthUnique, lengthNonUnique)
    helper (x:xs) countHash lu lnu = 
      case countHash Map.!? x of
        Nothing -> helper xs (Map.insert x False countHash) (lu+1) lnu
        Just False -> helper xs (Map.adjust (\_ -> True) x countHash) (lu-1) (lnu+1)
        Just True -> helper xs countHash lu lnu

cuanuNaive :: Eq a => [a] -> (Int, Int)
cuanuNaive xs = (length unique - length nonUnique, length nonUnique)
  where unique = nub xs
        nonUnique = nub $ xs \\ unique

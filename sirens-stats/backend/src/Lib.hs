module Lib
    ( countUniqueAndNonUnique
    , cuanuNaive
    , digits
    , toLuhnForm
    , isLuhn
    , sirenFromInt
    ) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

newtype Siren = Siren Integer 
    deriving (Eq, Show)

sirenFromInt :: Integer -> Maybe Siren
sirenFromInt x 
    | 0 <= x && x < 10^9 && isLuhn x = Just (Siren x)
    | otherwise = Nothing

isLuhn x = mod (sum $ toLuhnForm x) 10 == 0

toLuhnForm x = helper (reverse $ digits x) []
    where
        helper (a:b:xs) acc = helper xs (sumDigit (2*b):a:acc)
        helper [x] acc = x:acc
        helper [] acc = acc
        sumDigit x = if x > 9 then x-9 else x 

digits x = helper x []
    where
        helper 0 acc = acc
        helper x acc = helper q (r:acc)
            where (q, r) = quotRem x 10

countUniqueAndNonUnique l = helper l Map.empty 0 0
  where 
    helper [] _ lengthUnique lengthNonUnique = (lengthUnique, lengthNonUnique)
    helper (x:xs) countHash lu lnu = 
      case countHash Map.!? x of
        Nothing -> helper xs (Map.insert x False countHash) (lu+1) lnu
        Just False -> helper xs (Map.adjust (\_ -> True) x countHash) (lu-1) (lnu+1)
        Just True -> helper xs countHash lu lnu

cuanuNaive xs = (length unique - length nonUnique, length nonUnique)
  where unique = nub xs
        nonUnique = nub $ xs \\ unique

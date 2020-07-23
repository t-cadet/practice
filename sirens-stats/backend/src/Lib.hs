module Lib
    ( countUniqueAndNonUnique
    , cuanuNaive
    ) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

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

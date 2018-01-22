module DecisionTree where

import Control.Arrow

{-Classification And Dimensionality-}
data Group a = Group a
  deriving (Eq, Show, Ord)

type Point = (String, [Double])

groupWith :: Ord b => (a -> b) -> [a] -> [[a]] 
groupWith identifier l = groupBy ((==) `on` identifier) . sortBy (compare `on` identifier)

entropy :: [(String, Double)] -> Double
entropy separator dimension = 
  let groups = groupWith fst dimension
      frequency = map length groups 
      total = length dimension
      negativeEntropy = foldl (\l a -> a + (l / total) * logBase 2 (l / total)) 0 frequency
   in -1 * negativeEntropy 


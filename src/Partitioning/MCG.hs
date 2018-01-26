module Partitioning.MCG (Partitioning.MCG.partition) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function
import Utils

{-most common group-}
mostCommonGroup :: [Point] -> [Point] 
mostCommonGroup groups = 
  let groups' = groupOnName groups
      (group, _) = maximumBy (compare `on` snd) $ map (id &&& length) groups'
    in group 

{- Needs to be given isolated group-}
mostCommonAttribute :: Int -> [Point] -> Int
mostCommonAttribute dmn groups = minIndex $ map (attributeDelta groups) [0.. dmn] where
    attributeDelta :: [Point] -> Int -> Double
    attributeDelta gs i = let attr = map ((!!i) . snd) gs
                              (mx, mn) = maxMin attr 
                              in mx - mn
    

{- Partintioning Condition for Binary Tree, needs to be given the isolated group one is checking -} 
attributeDeltaPartition :: Int -> [Point] -> (Point -> Bool)
attributeDeltaPartition attribute groups =
  let attrs = map ((!! attribute) . snd) groups
      (mx, mn) = maxMin attrs in \(_, attrs') -> let attr = attrs' !! attribute 
                          in attr >= mn && attr <= mx



binaryDistributionMCG :: Int -> [Point] -> Point -> Bool
binaryDistributionMCG dmn groups = 
  let mcg = mostCommonGroup groups
      mca = mostCommonAttribute dmn mcg
    in attributeDeltaPartition mca mcg


partition :: Int -> ([Point] -> Point -> Bool)
partition = binaryDistributionMCG 

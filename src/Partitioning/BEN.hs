module Partitioning.BEN (Partitioning.BEN.partition) where


import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function
import Utils


{- Binary Entropy Minimization -}

bmn :: Int
bmn = 9 

dimensionMinMax :: [Point] -> Int -> (Double, Double)
dimensionMinMax groups dimension = 
  let scope = map ((!!dimension) . snd) groups
    in maxMin scope


partitioners :: [Point] -> Int -> [Point -> Bool]  
partitioners groups dimension = map (partitioning dimension) $ mx : mn : binaryGroups mx mn bmn
  where
    (mx, mn) = dimensionMinMax groups dimension

    binaryGroups :: Double -> Double -> Int -> [Double]
    binaryGroups _ _ 0 = []
    binaryGroups mx' mn' depth = 
      let mid = (mx' + mn') / 2
          lowerTier = mid : binaryGroups mid mn' (depth - 1) 
        in (lowerTier ++ binaryGroups mx' mid (depth - 1))



allAxisAlignedPartitioners :: Int -> [Point] -> [Point -> Bool]
allAxisAlignedPartitioners dmn groups = concatMap (partitioners groups) [0.. dmn]


partitionScore :: [Point] -> (Point -> Bool) -> (Double, Point -> Bool)
partitionScore groups p = 
  let (upper, lower) = partitionWith p groups 
    in (informationGain groups [upper, lower], p)

maxInformationGain :: Int -> [Point] -> Point -> Bool
maxInformationGain dmn groups = 
  let partitioners = allAxisAlignedPartitioners dmn groups
      scores = map (partitionScore groups) partitioners 
    in snd $ maximumBy (compare `on` fst) scores


partitioning :: Int -> Double -> Point -> Bool
partitioning dim part (_, dimensions) = dimensions !! dim > part

partition :: Int -> ([Point] -> Point -> Bool)
partition = maxInformationGain 


module Partitioning.MDR () where



{-{- Mean Difference Regression -}-}


{-import Control.Arrow-}
{-import Control.Applicative-}
{-import Control.Monad-}
{-import Data.List-}
{-import Data.Function-}
{-import Utils-}


{-meanDifference :: [Double] -> [Double] -> [Double]-}
{-meanDifference p1 p2 =-}
  {-let directionVector = zipWith (-) p1 p2-}
      {-distance =  sqrt . sum $ map (^2) directionVector-}
    {-in zipWith (\c1 c2 -> c1 + distance * c2) p1 p2-}


{-{- This is probably the bottleneck, or Matrix Multiplications and stuff -}-}

{-meanDifferenceSpace :: [[Double]] -> [[Double]] -> [[Double]]-}
{-meanDifferenceSpace group1 = concatMap mds1 -}
  {-where-}
    {-mds1 :: [Double] -> [[Double]]-}
    {-mds1 i2 = map (meanDifference i2) group1-}



{- This is supposed to generate coefficients for some kind of regession space
 - However i don't know what i'm doing but this felt good intuitivley
 - What i think i'm doing: Solving for the coefficients of the Hyper space which best 
 - intersects all the points given in m. 
 - What i'm actually doing: ??? 
 -
 - Side Note: The solution space, the vector filled with 1's, that's just a magic number, 0 is a 
 - bad Magic number though since all coefficients can be 0 in that case which just generates the empty space.. 
 - Which doesn't partition much at all-}


{-partitionSpace :: [[Double]] -> [Double]-}
{-partitionSpace m = -}
  {-let meanMatrix = transpose m `matrixMult` m -}
    {-in meanMatrix `matrixVectorMult` replicate (length meanMatrix) 1-}


{- This is Supposed to Determine wether a point is on either sides of the cool partition Hyper Space 
 - The Previous thingy was supposed to Generate
 - But does it work, and if it does, does it in the way i think? Who knows..-}

{-partitionFunc :: [Double] -> Point -> Bool-}
{-partitionFunc pspace (_, pos) = -}











{-partitioning :: [Point] -> Point -> Bool-}
{-partitioning group = const True-}




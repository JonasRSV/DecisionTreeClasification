module Utils where


import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function

instance Monoid Double where
  mempty = 0.0
  mappend = (+)

type Point = (String, [Double])

entropy ::  [String] -> Double
entropy labels = let tl = fromIntegral $ length labels
                     ent i = (i/tl) * logBase 2 (i/tl)
                  in negate . sum . map (ent . fromIntegral . length) . group $ sort labels 

informationGain :: [Point] -> [[Point]] -> Double
informationGain groups subgroups = 
  let entropySum = sum $ map (\subgroup -> (fromIntegral . length $ subgroup) * (entropy . classes $ subgroup)) subgroups
    in (entropy . classes $ groups) - entropySum / (fromIntegral . length $ groups)


minIndex :: (Ord a, Monoid a, Num a) => [a] -> Int
minIndex l = snd . foldl1 mini $ zip l [0..]
  where
    mini (a, i) (b, i') = case compare a b of
                LT -> (a, i)
                _ -> (b, i')

maxMin :: (Ord a) => [a] -> (a, a)
maxMin l = let mx = maximum l
               mn = minimum l
            in (mx, mn)

groupOnName :: [Point] -> [[(String, [Double])]]
groupOnName = groupBy ((==) `on` fst) . sortBy (compare `on` fst)



partitionWith :: (Point -> Bool) -> [Point] -> ([Point], [Point])
partitionWith f = foldr disperse ([], [])
  where
    disperse :: Point -> ([Point], [Point]) -> ([Point], [Point])
    disperse p (a1, a2) = if f p
                            then (p : a1, a2)
                            else (a1, p : a2)

classification :: [Point] -> String
classification groups = bodyResponse ++ "\n"
  where 
    classes = map (fst . head) $ groupOnName groups
    total = length groups

    occurrences :: [(String, Int)]
    occurrences = map (fst . head &&& length) $ groupOnName groups


    {-headerResponse = foldr (\a b -> b ++ " or " ++ a) (head classes) (tail classes) ++ "\n"-}
    bodyResponse = concatMap (\(name, freq) -> "- " ++ name ++ " " ++ show ((freq * 100) `quot` total) ++ "%") occurrences  


classes :: [Point] -> [String]
classes = map fst 


matrixVectorMult :: [[Double]] -> [Double] -> [Double]
matrixVectorMult m v = map (sum . zipWith (+) v) m

matrixMult :: [[Double]] -> [[Double]] -> [[Double]]
matrixMult m1 = transpose . map (matrixVectorMult m1) . transpose  




module DecisionTree where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function

instance Monoid Double where
  mempty = 0.0
  mappend = (+)


type Point = (String, [Double])


data DecisionTree = Tree (Point -> DecisionTree) | Leaf String


{- Most common group, does not take entropy into account, im not sure how to do that yet. -}


buildTreeBinary :: ([Point] -> Point -> Bool) -> [Point] -> DecisionTree
buildTreeBinary d groups = 
    case partitionWith distribution groups of
      ([], []) -> Leaf "Unclassifiable"
      ([], a) -> Leaf $ classification a
      (a, []) -> Leaf $ classification a
      (a, b) -> Tree $ \p -> if distribution p
                                then buildTreeBinary d a
                                else buildTreeBinary d b
  where distribution = d groups

queryTree :: DecisionTree -> Point -> String
queryTree (Leaf a) p = a
queryTree (Tree f) p = queryTree (f p) p


stdioQueriesNormal :: [Point] -> IO ()
stdioQueriesNormal state = 
  do
    putStrLn "Building Tree.."
    let tree = buildTreeMCG state
    putStrLn "Ready for Questions! "

    forever $ do
                query <- getLine 
                print $ queryTree tree ("", read query)



stdioQueriesFile :: (FilePath -> IO [Double]) -> [Point] -> IO ()
stdioQueriesFile mesher state =
  do
    putStrLn "Building Tree.."
    let tree = buildTreeMCG state
    putStrLn "Ready for Questions! "

    forever $ do
                file <- getLine
                mesh <- mesher file
                print $ queryTree tree ("", mesh)




{-Classification And Dimensionality-}


entropy ::  [String] -> Double
entropy labels = let tl = fromIntegral $ length labels
                     ent i = (i/tl) * logBase 2 (i/tl)
                  in negate . sum . map (ent . fromIntegral . length) . group $ sort labels 




{- BEN -> Binary Entropy minimization -}

{- BEN MAGIC NUMBER -}
bmn :: Int
bmn = 3

benDimensionScope :: [Point] -> Int -> (Double, Double)
benDimensionScope groups dimension = 
  let scope = map ((!!dimension) . snd) groups
    in maxMin scope

benScopePartition :: Int -> Double -> Point -> Bool
benScopePartition dim part (_, dimensions) = dimensions !! dim >= part

benGetDimPartitioners :: [Point] -> Int -> [Point -> Bool]  
benGetDimPartitioners groups dimension = map (benScopePartition dimension) $ mx : mn : binaryGroups mx mn bmn
  where
    (mx, mn) = benDimensionScope

    binaryGroups :: Double -> Double -> Int -> [Double]
    binaryGroups _ _ 0 = []
    binaryGroups mx' mn' depth = 
      let mid = (mx' + mn') / 2
          lowerTier = mid : binaryGroups mid mn' (depth - 1) 
        in (lowerTier ++ binaryGroups mx' mid (depth - 1))



benGetPartitioners :: [Point] -> [Point -> Bool]
benGetPartitioners groups =
  let dimensions = lenght . snd $ groups
    in concatMap (benGetDimPartitioners groups) [0.. dimensions]


partitionScore :: [Point] -> (Point -> Bool) -> (Double, (Point -> Bool))
partitionScore groups p = 
  let (upper, lower) = partitionWith p groups 
    in ((entropy . classes $ upper) + (entropy . classes $ lower), p)

benMinimizingPartitioner :: [Point] -> Point -> Bool
benMinimizingPartitioner groups = 
  let partitioners = benGetPartitioners groups
      scores = map (partitionScore groups) partitioners 
    in snd $ minimumBy (compare `on` fst) scores


buildTreeBEN :: [Point] -> DecisionTree
buildTreeBEN groups = buildTreeBinary minimizingPartitioner


{- \BEN -}



{- MCG Paritioning Works bad when Classes Share alot of attributes -}

{-most common group-}
mostCommonGroup :: [Point] -> [Point] 
mostCommonGroup groups = 
  let groups' = groupOnName groups
      (group, _) = maximumBy (compare `on` snd) $ map (id &&& length) groups'
    in group 

{- Needs to be given isolated group-}
mostCommonAttribute :: [Point] -> Int
mostCommonAttribute groups = minIndex $ map (attributeDelta groups) [0.. length groups] where
    attributeDelta :: [Point] -> Int -> Double
    attributeDelta gs i = let attr = map ((!!i) . snd) gs
                              (mx, mn) = maxMin attr in mx - mn
    

{- Partintioning Condition for Binary Tree, needs to be given the isolated group one is checking -} 
attributeDeltaPartition :: Int -> [Point] -> (Point -> Bool)
attributeDeltaPartition attribute groups =
  let attrs = map ((!! attribute) . snd) groups
      (mx, mn) = maxMin attrs in \(_, attrs') -> let attr = attrs' !! attribute 
                          in attr >= mn && attr <= mx



binaryDistributionMCG :: [Point] -> Point -> Bool
binaryDistributionMCG groups = 
  let mcg = mostCommonGroup groups
      mca = mostCommonAttribute mcg
    in attributeDeltaPartition mca mcg


buildTreeMCG :: [Point] -> DecisionTree
buildTreeMCG = buildTreeBinary binaryDistributionMCG

{- /MCG PARTITIONING -}


{- Utilities -}

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
classification groups = foldr (\a b -> b ++ " or " ++ a) (head classes) (tail classes) 
  where 
    classes = map (fst . head) $ groupOnName groups


classes :: [Point] -> [String]
classes = map fst 

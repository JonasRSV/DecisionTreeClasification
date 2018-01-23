module DecisionTree where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function

instance Monoid Double where
  mempty = 0.0
  mappend = (+)



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

{-Classification And Dimensionality-}
type Point = (String, [Double])


entropy ::  [String] -> Double
entropy labels = let tl = fromIntegral $ length labels
                     ent i = (i/tl) * logBase 2 (i/tl)
                  in negate . sum . map (ent . fromIntegral . length) . group $ sort labels 


{-most common group-}
mostCommonGroup :: [Point] -> [Point] 
mostCommonGroup groups = 
  let groups' = groupOnName groups
      (group, _) = maximumBy (compare `on` snd) $ map (id &&& length) groups'
    in group

{- Needs to be given isolated group-}
mostCommonAttribute :: [Point] -> Int
mostCommonAttribute groups = minIndex $ map (attributeDelta groups) [0.. length groups]
  where
    attributeDelta :: [Point] -> Int -> Double
    attributeDelta gs i = let attr = map ((!!i) . snd) gs
                              (mx, mn) = maxMin attr
                            in mx - mn
    

{- Partintioning Condition for Binary Tree, needs to be given the isolated group one is checking -} 
attributeDeltaPartition :: Int -> [Point] -> (Point -> Bool)
attributeDeltaPartition attribute groups =
  let attrs = map ((!! attribute) . snd) groups
      (mx, mn) = maxMin attrs
      in \(_, attrs') -> let attr = attrs' !! attribute 
                          in attr >= mn && attr <= mx

partitionWith :: (Point -> Bool) -> [Point] -> ([Point], [Point])
partitionWith f = foldr disperse ([], [])
  where
    disperse :: Point -> ([Point], [Point]) -> ([Point], [Point])
    disperse p (a1, a2) = if f p
                            then (p : a1, a2)
                            else (a1, p : a2)




data DecisionTree = Tree (Point -> DecisionTree) | Leaf String

{- Most common group, does not take entropy into account, im not sure how to do that yet. -}
binaryDistributionMCG :: [Point] -> (Point -> Bool)
binaryDistributionMCG groups = 
  let mcg = mostCommonGroup groups
      mca = mostCommonAttribute mcg
    in attributeDeltaPartition mca mcg


classification :: [Point] -> String
classification groups = foldr (\a b -> b ++ " or " ++ a) (head classes) (tail classes) 
  where 
    classes = map (fst . head) $ groupOnName groups

buildTreeMCG :: [Point] -> DecisionTree
buildTreeMCG groups = 
    case partitionWith distribution groups of
      ([], []) -> Leaf "Unclassifiable"
      ([], a) -> Leaf $ classification a
      (a, []) -> Leaf $ classification a
      (a, b) -> Tree $ \p -> if distribution p
                                then buildTreeMCG a
                                else buildTreeMCG b
  where
    distribution = binaryDistributionMCG groups



queryTree :: DecisionTree -> Point -> String
queryTree (Leaf a) p = a
queryTree (Tree f) p = queryTree (f p) p


stdioQueriesNormal :: [Point] -> IO ()
stdioQueriesNormal state = 
  do
    putStrLn "Building Tree.."
    let tree = buildTreeMCG state

    forever $ do
                query <- getLine 
                print $ queryTree tree ("", read query)



stdioQueriesFile :: (FilePath -> IO [Double]) -> [Point] -> IO ()
stdioQueriesFile mesher state =
  do
    putStrLn "Building Tree.."
    let tree = buildTreeMCG state

    forever $ do
                file <- getLine
                mesh <- mesher file
                print $ queryTree tree ("", mesh)



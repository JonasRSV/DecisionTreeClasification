module DecisionTree where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Function
import Utils

import Partitioning.BEN
import Partitioning.MCG



data DecisionTree = Tree (Point -> DecisionTree) | Leaf String




buildTreeBinary :: ([Point] -> Point -> Bool) -> [Point] -> DecisionTree
buildTreeBinary d groups = 
    case partitionWith distribution groups of
      ([], []) -> Leaf "Unclassifiable"
      ([], a) -> Leaf $ classification a
      (a, []) -> Leaf $ classification a
      (a, b) -> Tree $ \p -> if distribution p
                                then wanderTree (buildTreeBinary d a) p
                                else wanderTree (buildTreeBinary d b) p
  where distribution = d groups


wanderTree :: DecisionTree -> Point -> DecisionTree
wanderTree (Tree f) p = wanderTree (f p) p
wanderTree leaf@Leaf{} _ = leaf

queryTree :: DecisionTree -> Point -> String
queryTree (Tree f) p = case f p of
                        Leaf c -> c
                        Tree {} -> error "Query Tree did not Reach Leaf"

queryTree (Leaf a) _ = a


stdioQueriesNormal :: DecisionTree -> IO ()
stdioQueriesNormal tree = 
    forever $ do
                query <- getLine 
                putStr $ queryTree tree ("", read query)



stdioQueriesFile :: DecisionTree -> (FilePath -> IO [Double]) -> IO ()
stdioQueriesFile tree mesher = 
    forever $ do
                file <- getLine
                mesh <- mesher file
                putStr $ "** " ++ file ++ " ** " ++ queryTree tree ("", mesh)










module Main where

import DecisionTree
import BMPPROC
import qualified System.IO.Strict as Strict
import Control.Monad
import System.Environment

{- This is the Dependency -}
bmpMesher :: Int -> FilePath -> IO [Double]
bmpMesher = meshImage meanValueGrouping

memoryName :: FilePath
memoryName = "classifier.mem"

type Memory = (Int, [Point])

initMemory :: Point -> IO ()
initMemory p = 
  do
    putStrLn $ "Dimensionality Set to: " ++ show (length . snd $ p)
    writeFile memoryName . show $ (length . snd $ p, [p])

addClassification :: Point -> IO ()
addClassification point = 
  do
    (dim, state) <- (read <$> Strict.readFile memoryName) :: IO Memory
    if (length . snd $ point) == dim
      then writeFile memoryName . show $ (dim, point : state)
      else putStrLn $ "Cannot add because dimensionality does not match\nMemory: " ++ show dim ++ "\nTried to add: " ++ show (length . snd $ point)

addBMP :: Int -> String -> FilePath -> IO ()
addBMP dimensionality classification file = 
  do
    mesh <- bmpMesher dimensionality file
    addClassification (classification, mesh)

addByHand :: String -> [Double] -> IO ()
addByHand name mesh = addClassification (name, mesh)



normalAddSession :: IO ()
normalAddSession = forever $
  do
    putStrLn "(Classifier, Dimensions)"
    (classi, dims) <- (read <$> getLine) :: IO Point
    addByHand classi dims

bmpAddSession :: IO ()
bmpAddSession = 
  do
    (dim, _) <- (read <$> Strict.readFile memoryName) :: IO Memory
    putStrLn "Classifier FilePath"

    forever $ 
      do
        [classi, filepath] <- words <$> getLine
        addBMP dim classi filepath

bmpInit :: IO ()  
bmpInit = 
  do
    putStrLn "Classifier FilePath Dimensionality"
    [classi, filepath, dim] <- words <$> getLine
    mesh <- bmpMesher (read dim) filepath
    initMemory (classi, mesh)

normalInit :: IO ()
normalInit =
  do
    putStrLn "(Classifier, Dimensions)"
    p <- (read <$> getLine) :: IO Point
    initMemory p


normalQuerySession :: (Int -> [Point] -> DecisionTree) -> IO ()
normalQuerySession d =
  do
    (dim, mem) <- (read <$> Strict.readFile memoryName) :: IO Memory
    putStrLn "Building Tree"
    let tree = d (dim - 1) mem
    putStrLn "Ready for Queries"
    putStrLn $ "Remember to keep dimensionality at: " ++ show dim ++ "\n"
    stdioQueriesNormal tree


bmpQuerySession :: (Int -> [Point] -> DecisionTree) -> IO ()
bmpQuerySession d =
  do
    (dim, mem) <- (read <$> Strict.readFile memoryName) :: IO Memory
    let mesher = bmpMesher dim

    putStrLn "Building Tree"
    let tree = d (dim - 1) mem
    putStrLn "Ready for Queries"
    stdioQueriesFile tree mesher 


main :: IO ()
main = 
  do
    args <- getArgs
    case args of
      [] -> putStr "Welcome to Tree!\n\nInit Normal: -i\nAdd Normal: -a\nQuery Normal: -q\nInit Image: -im\nAdd Image: -am\nQuery Image: -qm\nQuery using MeanValueGrouping: Image: -qgm, Normal -qg"
      ("-i":_) -> normalInit
      ("-a":_) -> normalAddSession
      ("-q":_) -> normalQuerySession buildTreeBEN
      ("-qg":_) -> normalQuerySession buildTreeMCG
      ("-im":_) -> bmpInit
      ("-am":_) -> bmpAddSession
      ("-qm":_) -> bmpQuerySession buildTreeBEN
      ("-qgm":_) -> bmpQuerySession buildTreeMCG

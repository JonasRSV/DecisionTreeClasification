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
initMemory p = writeFile memoryName . show $ (length . snd $ p, [p])

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


normalQuerySession :: IO ()
normalQuerySession =
  do
    (dim, mem) <- (read <$> Strict.readFile memoryName) :: IO Memory

    putStrLn $ "Remember to keep dimensionality at: " ++ show dim ++ "\n"
    stdioQueriesNormal mem


bmpQuerySession :: IO ()
bmpQuerySession =
  do
    (dim, mem) <- (read <$> Strict.readFile memoryName) :: IO Memory
    let mesher = bmpMesher dim
    stdioQueriesFile mesher mem

normalAddSession :: IO ()
normalAddSession = forever $
  do
    (classi, dims) <- (read <$> getLine) :: IO Point
    addByHand classi dims

bmpAddSession :: IO ()
bmpAddSession = 
  do
    (dim, _) <- (read <$> Strict.readFile memoryName) :: IO Memory

    forever $ 
      do
        [classi, filepath] <- words <$> getLine
        addBMP dim classi filepath

bmpInit :: IO ()  
bmpInit = 
  do
    [classi, filepath, dim] <- words <$> getLine
    mesh <- bmpMesher (read dim) filepath
    initMemory (classi, mesh)

normalInit :: IO ()
normalInit =
  do
    p <- (read <$> getLine) :: IO Point
    initMemory p


main :: IO ()
main = 
  do
    args <- getArgs
    case args of
      [] -> putStr "Welcome to Tree!\n\nInit Normal: -i\nAdd Normal: -a\nQuery Normal: -q\nInit Image: -im\nAdd Image: -am\nQuery Image: -qm"
      ("-i":_) -> normalInit
      ("-a":_) -> normalAddSession
      ("-q":_) -> normalQuerySession
      ("-im":_) -> bmpInit
      ("-am":_) -> bmpAddSession
      ("-qm":_) -> bmpQuerySession

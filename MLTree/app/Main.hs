module Main where

import KMeans
import KMeansVis (visKMeans)
import Data.Csv (decode, encodeByName, HasHeader( NoHeader, HasHeader ))
import Text.Pretty.Simple (pPrint)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Vector (Vector, toList, (!))
import Sequence
import Coords
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = someFunc

someFunc :: IO ()
someFunc = do
  x <- readFile "coords_2021.csv"
  let eitherCoords = (decode HasHeader (fromString x) :: Either String (Vector Coords)) in do
    initCts3 <- sequence (initCts 3 <$> eitherCoords)
    initCts4 <- sequence (initCts 4 <$> eitherCoords)
    initCts5 <- sequence (initCts 5 <$> eitherCoords)
    display (combine <$> (sequence [visKMeans 3 <$> eitherCoords <*> initCts3,
                                    visKMeans 4 <$> eitherCoords <*> initCts4,
                                    visKMeans 5 <$> eitherCoords <*> initCts5])) where
      display (Left s) = pPrint s
      display (Right s) = writeFile "kmeans_vis.csv" (toString (encodeByName (constructHeader (s ! 0)) (toList s)))

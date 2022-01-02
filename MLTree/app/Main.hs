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
  let initCts' = initCts 3 <$> (decode HasHeader (fromString x) :: Either String (Vector Coords)) in
    display (visKMeans 3 <$> (decode HasHeader (fromString x) :: Either String (Vector Coords)) <*> (initCts' >>= Right . unsafePerformIO)) where
      display (Left s) = pPrint s
      display (Right s) = do
        writeFile "kmeans_vis.csv" (toString (encodeByName (constructHeader (s ! 0)) (toList s)))

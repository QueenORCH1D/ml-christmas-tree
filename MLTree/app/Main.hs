module Main where

import KMeans
import KMeansVis (visKMeans)
import Data.Csv (decode, encodeByNameWith, HasHeader( NoHeader, HasHeader ))
import Text.Pretty.Simple (pPrint)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Vector (Vector, toList, (!))
import System.Random (getStdGen)
import Sequence
import Coords
import System.IO.Unsafe (unsafePerformIO)
import Constants

-- Takes coordinates from coords.csv, generates a visualisation for k-means
-- clustering on those coordinates with parameters taken from Constants.hs,
-- then outputs that to kmeans_vis.csv
main :: IO ()
main = do
  x <- readFile "coords_2021.csv"
  let eitherCoords = (decode HasHeader (fromString x) :: Either String (Vector Coords))
  g <- getStdGen
  let initCtss = zipWith (<$>) (initCts g <$> ks) (replicate (length ks) eitherCoords)
  display (combine <$> (sequence (zipWith (zipFunc eitherCoords) (map visKMeans ks) initCtss)))
  where
    display (Left s) = pPrint s
    display (Right s) = writeFile "kmeans_vis.csv" (toString (encodeByNameWith encodeOptions (constructHeader (s ! 0)) (toList s)))
    zipFunc eitherCoords x y = x <$> eitherCoords <*> y

module Main where

import KMeans
import KMeansVis (visKMeans)
import Data.Csv (decode, encodeByNameWith, HasHeader( NoHeader, HasHeader ))
import Text.Pretty.Simple (pPrint)
import Data.ByteString.Lazy.UTF8 (ByteString, fromString, toString)
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.ByteString.Lazy.UTF8 as B (break)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy as T (filter, replace)
import Data.Vector (Vector, toList, (!))
import System.Random (getStdGen)
import Sequence
import Coords
import System.IO.Unsafe (unsafePerformIO)
import Constants
import Data.Char (isAscii)

-- Takes coordinates from coords.csv, generates a visualisation for k-means
-- clustering on those coordinates with parameters taken from Constants.hs,
-- then outputs that to kmeans_vis.csv
main :: IO ()
main = do
  x <- B.readFile "coords_2021.csv"
  let eitherCoords = (decode NoHeader (standardise x) :: Either String (Vector Coords))
  g <- getStdGen
  let initCtss = zipWith (<$>) (initCts g <$> ks) (replicate (length ks) eitherCoords)
  display (combine <$> (sequence (zipWith (zipFunc eitherCoords) (map visKMeans ks) initCtss)))
  where
    display (Left s) = pPrint s
    display (Right s) = writeFile "kmeans_vis.csv" (toString (encodeByNameWith encodeOptions (constructHeader (s ! 0)) (toList s)))
    zipFunc eitherCoords x y = x <$> eitherCoords <*> y--}



standardise :: ByteString -> ByteString
standardise x = encodeUtf8 (removeXd (T.filter isAscii (decodeUtf8 x))) where
  removeXd = T.replace (pack "\xd") (pack"")

module KMeans
    ( kMeans, vKMeans, kClusters, initCts
    ) where
import Data.Vector (Vector, singleton)
import qualified Data.Vector as V (filter, map, snoc, accumulate, replicate, empty, minIndex, maximum, zipWith)
import System.Random
import Coords

-- Verbose version of the kMeans function. Will output the centroids at every iteration
-- The number of iterations is dependant on the number of centroids, as models with
-- more centroids could take longer to converge.
vKMeans :: Int -> Vector Coords -> Vector Coords -> [Vector Coords]
vKMeans k initCts' coords = do rVKMeans (6*k) k initCts' coords

-- Recursive version of the verbose kMeans function
rVKMeans :: Int -> Int -> Vector Coords -> Vector Coords -> [Vector Coords]
rVKMeans 0 _ cts _ = [cts]
rVKMeans n k cts coords = cts : (rVKMeans (n-1) k cts' coords) where
  cts' = centroids k (kClusters k coords cts)

-- Performs k-means clustering on data when given k, a vector of coordinates that
-- represents the data, and the initial centroids. Outputs the final centroids as
-- a vector of length k.
kMeans :: Int -> Vector Coords -> Vector Coords -> Vector Coords
kMeans k coords initCts' = rKMeans (6*k) k initCts' coords

-- Used to initialise the centroids. Generates k points in GIFT format, where the
-- height is the maximum height given.
initCts :: StdGen -> Int -> Vector Coords -> Vector Coords
initCts g k coords = packCoords (take (3*k) (randomRs (-1.0, 1.0) g)) where
    packCoords [] = V.empty
    packCoords (x:y:z:xs) = V.snoc (packCoords xs) (mkCoords x y (((z + 1)/2)*maxZ))
    maxZ = V.maximum (V.map getZ coords)

-- Recursive version of the kMeans function
rKMeans :: Int -> Int -> Vector Coords -> Vector Coords -> Vector Coords
rKMeans 0 _ cts _ = cts
rKMeans n k cts coords = centroids k (kClusters k coords (rKMeans (n-1) k cts coords))

-- Cluster the data into k clusters based on the closest centroid
kClusters :: Int -> Vector Coords -> Vector Coords -> Vector (Int, Coords)
kClusters k coords cts = V.map (\x -> (findCluster x cts, x)) coords

-- Given a point and a vector of centroids, find the cluster that point is in.
findCluster :: Coords -> Vector Coords -> Int
findCluster x cts = V.minIndex distances where
  distances = V.map (dist x) cts

-- Calculate centroids from clustered data
centroids :: Int -> Vector (Int, Coords) -> Vector Coords
centroids k clusters = V.zipWith (/) sums lengths where
  sums = V.accumulate (+) (V.replicate k (single 0.0)) clusters
  lengths = V.accumulate (+) (V.replicate k (single 0.0)) (V.map (\(x,_) -> (x,single 1.0)) clusters)

module KMeansVis(
  visKMeans
) where
import Data.Vector (Vector, singleton)
import qualified Data.Vector as V (replicate, filter, zipWith, indexed, empty, map, (!), snoc, cons, (++), update_, minimumBy, fromList)
import Sequence
import Coords
import KMeans
import Constants

-- Takes clustered data and colours the clusters according to the parameters
-- specified in Constants.hs
colourCluster :: Int -> Vector Colour -> Vector (Int, Coords) -> Vector Coords -> Vector (Colour, Coords)
colourCluster k colours clusters cts = colourCenters k defCentreColour clusters (V.map (\(n, x) -> (colours V.! n, x)) clusters) cts

-- Updates coloured clusters with the point that is closest to the centroid coloured
-- in the given colour
colourCenters :: Int -> Colour -> Vector (Int, Coords) -> Vector (Colour, Coords) -> Vector Coords -> Vector (Colour, Coords)
colourCenters k colour clusters colours cts = V.update_ colours indexes (colourCenter colours) where
  indexes = getClosestToCentre k (calcDistFromCent clusters cts)
  colourCenter colourCoords = V.map (\(_, coords) -> (colour, coords)) (V.map ((V.!) clusters) indexes)

-- Gets a vector of the indexes of the point that is closest to the centroid in
-- each of the clusters
getClosestToCentre :: Int -> Vector (Int, Float) -> Vector Int
getClosestToCentre k clusters = V.map (fst . V.minimumBy compareClustDist) (partitionClusters k clusters)

-- Takes 2 indexed, clustered distances and compares them
compareClustDist :: (Int, (Int, Float)) -> (Int, (Int, Float)) -> Ordering
compareClustDist x y = compare (snd(snd x)) (snd (snd y))

-- Takes clustered data and splits it into vectors that each contain a single cluster,
-- and indexes the data for reconstruction
partitionClusters :: Int -> Vector (Int, a) -> Vector (Vector (Int, (Int, a)))
partitionClusters k clusters = V.zipWith (\x -> V.filter ((==) x . fst . snd)) (V.fromList [0..(k-1)]) (V.replicate k (V.indexed clusters))

-- Calculates the distance of each data point from its centroid and stores it
-- in the data point
calcDistFromCent :: Vector (Int, Coords) -> Vector Coords -> Vector (Int, Float)
calcDistFromCent clusters cts = V.map (\(x, y) -> (x, dist (cts V.! x) y)) clusters

-- Takes k, a set of data points as coordinates, and a set of initial centroids,
-- and produces a sequence that visualises the iterations of k-means clustering
-- on the data
visKMeans :: Int -> Vector Coords -> Vector Coords -> Sequence
visKMeans k coords initCts' = coloursSeq (zipWith ($) (map (colourCluster k (V.fromList defColours)) (map (kClusters k coords) (means))) means) where
  means = vKMeans k initCts' coords

-- Takes a vector of colours and coordinates and turns them into a sequence
coloursSeq :: [Vector (Colour, Coords)] -> Sequence
coloursSeq colouring = mkSequence (concatMap (replicate speed) (map (fst <$>) colouring))

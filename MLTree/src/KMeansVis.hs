module KMeansVis(
  visKMeans
) where
import Data.Vector (Vector, singleton)
import qualified Data.Vector as V (replicate, filter, zipWith, indexed, empty, map, (!), snoc, cons, (++), update_, minimumBy, fromList)
import Sequence
import Coords
import KMeans
import Constants

colourCluster :: Int -> Vector Colour -> Vector (Int, Coords) -> Vector Coords -> Vector (Colour, Coords)
colourCluster k colours clusters cts = colourCenters k defCentreColour clusters (V.map (\(n, x) -> (colours V.! n, x)) clusters) cts

colourCenters :: Int -> Colour -> Vector (Int, Coords) -> Vector (Colour, Coords) -> Vector Coords -> Vector (Colour, Coords)
colourCenters k colour clusters colours cts = V.update_ colours indexes (colourCenter colours) where
  indexes = getClosestToCentre k (calcDistFromCent clusters cts)
  colourCenter colourCoords = V.map (\(_, coords) -> (colour, coords)) (V.map ((V.!) clusters) indexes)


getClosestToCentre :: Int -> Vector (Int, Float) -> Vector Int
getClosestToCentre k clusters = V.map (fst . V.minimumBy compareClustDist) (partitionClusters k clusters)

compareClustDist :: (Int, (Int, Float)) -> (Int, (Int, Float)) -> Ordering
compareClustDist x y = compare (snd(snd x)) (snd (snd y))

partitionClusters :: Int -> Vector (Int, a) -> Vector (Vector (Int, (Int, a)))
partitionClusters k clusters = V.zipWith (\x -> V.filter ((==) x . fst . snd)) (V.fromList [0..(k-1)]) (V.replicate k (V.indexed clusters))

calcDistFromCent :: Vector (Int, Coords) -> Vector Coords -> Vector (Int, Float)
calcDistFromCent clusters cts = V.map (\(x, y) -> (x, dist (cts V.! x) y)) clusters

visKMeans :: Int -> Vector Coords -> Vector Coords -> Sequence
visKMeans k coords initCts' = coloursSeq (zipWith ($) (map (colourCluster k (V.fromList defColours)) (map (kClusters k coords) (means))) means) where
  means = vKMeans k initCts' coords

coloursSeq :: [Vector (Colour, Coords)] -> Sequence
coloursSeq colouring = mkSequence (concatMap (replicate speed) (map (fst <$>) colouring))

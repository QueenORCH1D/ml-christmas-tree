module KMeans
    ( kmeans, someFunc
    ) where
import Data.Csv (decode, HasHeader( NoHeader, HasHeader ), ToRecord (toRecord), FromRecord (parseRecord), (.!), record, toField)
import Data.Text (Text)
import Data.Vector (Vector, singleton)
import qualified Data.Vector as V (filter, map, snoc, accumulate, replicate, empty, minIndex, maximum, zipWith)
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Monad (mzero)
import System.Random

someFunc :: IO ()
someFunc = do
  x <- readFile "coords_2021.csv"
  display (kmeans 3 <$> (decode HasHeader (fromString x) :: Either String (Vector Coords)))  where
    display (Left s) = print s
    display (Right s) = do
      clusters <- s
      print clusters

kmeans :: Int -> Vector Coords -> IO (Vector Coords)
kmeans k coords = do
  initCts' <- initCts k coords
  return (rKMeans 15 k initCts' coords)

initCts :: Int -> Vector Coords -> IO (Vector Coords)
initCts k coords = do
  g <- newStdGen
  return (packCoords (take (3*k) (randomRs (-1.0, 1.0) g))) where
    packCoords [] = V.empty
    packCoords (x:y:z:xs) = V.snoc (packCoords xs) (Coords x y (((z + 1)/2)*maxZ))
    maxZ = V.maximum (V.map getZ coords)

rKMeans :: Int -> Int -> Vector Coords -> Vector Coords -> Vector Coords
rKMeans 0 _ cts _ = cts
rKMeans n k cts coords = centroids k (kClusters k coords (rKMeans (n-1) k cts coords))

-- Cluster the data into k clusters based on the closest centroid
kClusters :: Int -> Vector Coords -> Vector Coords -> Vector (Int, Coords)
kClusters k coords cts = V.map (\x -> (findCluster x cts, x)) coords

findCluster :: Coords -> Vector Coords -> Int
findCluster x cts = V.minIndex distances where
  distances = V.map (dist x) cts

-- Calculate centroids from clustered data
centroids :: Int -> Vector (Int, Coords) -> Vector Coords
centroids k clusters = V.zipWith (/) sums lengths where
  sums = V.accumulate (+) (V.replicate k (single 0.0)) clusters
  lengths = V.accumulate (+) (V.replicate k (single 0.0)) (V.map (\(x,_) -> (x,single 1.0)) clusters)

dist :: Coords -> Coords -> Float
dist (Coords x' y' z') (Coords x'' y'' z'') = sqrt (((x' - x'')**2) + ((y' - y'')**2) + ((z' - z'')**2))


data Coords = Coords {x, y, z :: Float}
  deriving(Show)

instance FromRecord Coords where
  parseRecord v
    | length v == 3 = Coords <$>
                      v .! 0 <*>
                      v .! 1 <*>
                      v .! 2
    | otherwise = mzero
instance ToRecord Coords where
  toRecord (Coords x' y' z') = record [toField x', toField y', toField z']

instance Num Coords where
  (+) (Coords x' y' z') (Coords x'' y'' z'') = Coords (x' + x'') (y' + y'') (z' + z'')
  (*) (Coords x' y' z') (Coords x'' y'' z'') =  Coords (x' * x'') (y' * y'') (z' * z'')
  abs (Coords x' y' z') = Coords (abs x') (abs y') (abs z')
  signum (Coords x' y' z') = Coords (signum x') (signum y') (signum z')
  negate (Coords x' y' z') = Coords (negate x') (negate y') (negate z')
  fromInteger n = single (fromInteger n)

instance Fractional Coords where
  fromRational r = single (fromRational r)
  (/) (Coords x' y' z') (Coords x'' y'' z'') = Coords (x' / x'') (y' / y'') (z' / z'')

single :: Float -> Coords
single x' = Coords x' x' x'

getZ :: Coords -> Float
getZ (Coords _ _ z) = z

mkCoords :: Float -> Float -> Float -> Coords
mkCoords x y z = Coords x y z

testCts :: Vector Coords
testCts = V.snoc (V.snoc (singleton (single 0.0)) (single 1.0)) (single (-1.0))

testCoords :: Vector Coords
testCoords = V.snoc (V.snoc (singleton (single 0.01)) (single 0.99)) (single (-0.99))

testClusters :: Vector (Int, Coords)
--testClusters = singleton (0, single 0.01)
testClusters = V.snoc (singleton (0, single 0.01)) (1, single 0.99)
--testClusters = V.snoc (V.snoc (singleton (0, single 0.01)) (1, single 0.99)) (2, single (-0.99))

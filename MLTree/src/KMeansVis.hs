module KMeansVis(
  visKMeans
) where
import Data.Vector (Vector, singleton)
import qualified Data.Vector as V (empty, map, (!), snoc, cons, (++))
import Sequence
import Coords
import KMeans

colourCluster :: Vector Colour -> Vector (Int, Coords) -> Vector (Colour, Coords)
colourCluster colours = V.map (\(n, x) -> (colours V.! n, x))

visKMeans :: Int -> Vector Coords -> Vector Coords -> Sequence
visKMeans k coords initCts' = coloursSeq (map (colourCluster defColours) (map (kClusters k coords) (vKMeans k initCts' coords)))

coloursSeq :: [Vector (Colour, Coords)] -> Sequence
coloursSeq colouring = mkSequence (concatMap (replicate 8) (map (fst <$>) colouring))

defColours :: Vector Colour
defColours = V.snoc (V.snoc (V.snoc (V.snoc (singleton (255, 0, 255)) (0, 255, 255)) (255, 0, 0)) (0, 255, 0)) (0, 0, 255)

combine :: [Sequence] -> Sequence
combine [] = V.empty
combine (seq':seqs) = seq' V.++ (combine (map (V.map (bumpFrame ((getFrame (seq' V.! 0)) + 1))) seqs))

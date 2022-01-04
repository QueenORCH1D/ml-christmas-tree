module Coords(
  Coords,
  single,
  getZ,
  mkCoords,
  dist) where
import Data.Csv (FromRecord (parseRecord), ToRecord (toRecord), (.!), record, toField)
import Control.Monad (mzero)

-- Contains 3D coordinates in space. Has values x, y and z.
data Coords = Coords {x, y, z :: Float}
  deriving(Show)

-- Used to encode a csv as coordinates
instance FromRecord Coords where
  parseRecord v
    | length v == 3 = Coords <$>
                      v .! 0 <*>
                      v .! 1 <*>
                      v .! 2
    | otherwise = mzero

-- Used to decode coordinates to a csv
instance ToRecord Coords where
  toRecord (Coords x' y' z') = record [toField x', toField y', toField z']

-- Used to perform maths on coordinates. This essentially zips the 2 coordinates
-- together with the functions
instance Num Coords where
  (+) (Coords x' y' z') (Coords x'' y'' z'') = Coords (x' + x'') (y' + y'') (z' + z'')
  (*) (Coords x' y' z') (Coords x'' y'' z'') =  Coords (x' * x'') (y' * y'') (z' * z'')
  abs (Coords x' y' z') = Coords (abs x') (abs y') (abs z')
  signum (Coords x' y' z') = Coords (signum x') (signum y') (signum z')
  negate (Coords x' y' z') = Coords (negate x') (negate y') (negate z')
  fromInteger n = single (fromInteger n)

-- Used to divide coordinates. Same logic as above.
instance Fractional Coords where
  fromRational r = single (fromRational r)
  (/) (Coords x' y' z') (Coords x'' y'' z'') = Coords (x' / x'') (y' / y'') (z' / z'')

-- Creates a coordinate from a single value. This'll set the x,y and z of the
-- coordinate to that value.
single :: Float -> Coords
single x' = Coords x' x' x'

-- Gets the Z value of a coordinate
getZ :: Coords -> Float
getZ (Coords _ _ z) = z

-- Constructs coordinates from given x, y and z values
mkCoords :: Float -> Float -> Float -> Coords
mkCoords x y z = Coords x y z

-- Calculates the euclidiean distance between 2 coordinates.
dist :: Coords -> Coords -> Float
dist (Coords x' y' z') (Coords x'' y'' z'') = sqrt (((x' - x'')**2) + ((y' - y'')**2) + ((z' - z'')**2))

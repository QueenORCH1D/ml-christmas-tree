module Coords(
  Coords,
  single,
  getZ,
  mkCoords,
  dist) where
import Data.Csv (FromRecord (parseRecord), ToRecord (toRecord), (.!), record, toField)
import Control.Monad (mzero)

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

dist :: Coords -> Coords -> Float
dist (Coords x' y' z') (Coords x'' y'' z'') = sqrt (((x' - x'')**2) + ((y' - y'')**2) + ((z' - z'')**2))

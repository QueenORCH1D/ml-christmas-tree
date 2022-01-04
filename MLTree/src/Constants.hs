module Constants(
  defColours,
  defCentreColour,
  speed
) where

import Data.Vector (Vector)
import qualified Data.Vector as V (fromList)
import Sequence

-- The colours that you want each of the clusters to be in RGB format.
-- If k is less than the size of this list, the first k will be used
defColours :: Vector Colour
defColours = V.fromList [(200, 0, 200), (0, 200, 200), (200, 0, 0), (0, 200, 0), (0, 0, 200)]

-- The colour you want the centres of the clusters to be in RGB format
defCentreColour :: Colour
defCentreColour = (255, 255, 0)

-- How fast you want the animation to play. Higher numbers are slower.
speed :: Int
speed = 8

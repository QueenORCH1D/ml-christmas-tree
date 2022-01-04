module Constants(
  defColours,
  defCentreColour,
  speed,
  ks,
  encodeOptions
) where

import Sequence
import Data.Csv (EncodeOptions(encDelimiter, encUseCrLf, encIncludeHeader, encQuoting), defaultEncodeOptions)

-- The colours that you want each of the clusters to be in RGB format.
-- If k is less than the size of this list, the first k will be used
defColours :: [Colour]
defColours = [(200, 0, 200), (0, 200, 200), (200, 0, 0), (0, 200, 0), (0, 0, 200), (200, 100, 0), (100, 0, 200)]

-- The colour you want the centres of the clusters to be in RGB format
defCentreColour :: Colour
defCentreColour = (255, 255, 0)

-- How fast you want the animation to play. Higher numbers are slower. Minimum 1
speed :: Int
speed = 8

-- The values of K that you want to visualise.
-- Each element in this list needs to be less than the number of colours given
ks :: [Int]
ks = [3,5,7]

--Changes the options for how data is encoded to a CSV (See Data.Csv EncodeOptions)
encodeOptions :: EncodeOptions
encodeOptions = defaultEncodeOptions {
  encUseCrLf = False
}

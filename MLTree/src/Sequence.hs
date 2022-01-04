module Sequence (
  Sequence,
  Step,
  Colour,
  step,
  mkSequence,
  constructHeader,
  bumpFrame,
  getFrame,
  combine
) where

import Data.Vector (Vector, indexed, fromList)
import qualified Data.HashMap.Internal as HM (insert, fromList)
import qualified Data.Vector as V (cons, concatMap, toList, length, map, (!), (++), empty)
import Data.Csv (FromRecord (parseRecord), ToRecord (toRecord), (.!), record, toField, ToField (toField), ToNamedRecord(toNamedRecord), (.=), Header)
import Data.ByteString.UTF8 (fromString, toString)
import Data.ByteString (append)

-- Used to format the output of the program. Represents the entire CSV file.
type Sequence = Vector Step

-- Individual records in the CSV file, with the frame ID, and the colours of each
-- of the lights
data Step = Step {frame :: Int, lights :: Vector Colour}
  deriving (Show)

-- Used for encoding sequences as CSV
instance ToRecord Step where
  toRecord (Step frame lights) = V.cons (toField frame) (toField <$> (V.concatMap (\(x, y, z) -> fromList [x, y, z]) lights))

-- Used for encoding sequences as a CSV with a header
instance ToNamedRecord Step where
  toNamedRecord (Step frame lights) = HM.insert (fromString "FRAME_ID") (fromString (show frame)) (HM.fromList coloursList) where
    coloursList = V.toList (V.concatMap colourPairs (indexed lights))
    colourPairs (x, (r, g, b)) = fromList [(fromString ("R_" ++ (show x))) .= r,
                                  (fromString ("G_" ++ (show x))) .= g,
                                  (fromString ("B_" ++ (show x))) .= b]

-- Used to construct a step
step :: Int -> Vector Colour -> Step
step = Step

-- Used to make generate a sequence from a list of colours. It'll automatically
-- set the frame ID of each step to the associated colour's index in the list
mkSequence :: [Vector Colour] -> Sequence
mkSequence lightsList = (uncurry step) <$> indexed (fromList lightsList)

-- Constructs a header for a given step. Used for the ordering of fields in the
-- final outputted CSV, as Data.Csv's encodeByWith uses an unordered HashMap.
constructHeader :: Step -> Header
constructHeader (Step frame lights) = V.cons (fromString "FRAME_ID") colourVector where
  colourVector = V.concatMap (\x -> fromList [fromString ("R_" ++ (show x)), fromString ("G_" ++ (show x)), fromString ("B_" ++ (show x))]) (fromList [0..((length lights)-1)])

-- Increases the frame ID of a step by n. Used to insert steps into a sequence in
-- combine
bumpFrame :: Int -> Step -> Step
bumpFrame n (Step frame lights) = Step (frame + n) lights

-- Gets the frame ID from a step
getFrame :: Step -> Int
getFrame (Step frame _) = frame

-- Combines a list of sequences into a single sequence. It'll bump the frame IDs
-- of the sequences so that there's no duplicate frame IDs and the sequences play
-- one after the other.
combine :: [Sequence] -> Sequence
combine [] = V.empty
combine (seq':seqs) = seq' V.++ (combine (map (V.map (bumpFrame ((getFrame (seq' V.! 0)) + 1))) seqs))

-- Type alias for storing colours in RGB format.
type Colour = (Int, Int, Int)

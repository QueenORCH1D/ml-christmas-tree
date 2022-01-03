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

type Sequence = Vector Step

data Step = Step {frame :: Int, lights :: Vector Colour}
  deriving (Show)

instance ToRecord Step where
  toRecord (Step frame lights) = V.cons (toField frame) (toField <$> (V.concatMap (\(x, y, z) -> fromList [x, y, z]) lights))

instance ToNamedRecord Step where
  toNamedRecord (Step frame lights) = HM.insert (fromString "FRAME_ID") (fromString (show frame)) (HM.fromList coloursList) where
    coloursList = V.toList (V.concatMap colourPairs (indexed lights))
    colourPairs (x, (r, g, b)) = fromList [(fromString ("R_" ++ (show x))) .= r,
                                  (fromString ("G_" ++ (show x))) .= g,
                                  (fromString ("B_" ++ (show x))) .= b]

step :: Int -> Vector Colour -> Step
step = Step

mkSequence :: [Vector Colour] -> Sequence
mkSequence lightsList = (uncurry step) <$> indexed (fromList lightsList)

constructHeader :: Step -> Header
constructHeader (Step frame lights) = V.cons (fromString "FRAME_ID") colourVector where
  colourVector = V.concatMap (\x -> fromList [fromString ("R_" ++ (show x)), fromString ("G_" ++ (show x)), fromString ("B_" ++ (show x))]) (fromList [0..((length lights)-1)])

bumpFrame :: Int -> Step -> Step
bumpFrame n (Step frame lights) = Step (frame + n) lights

getFrame :: Step -> Int
getFrame (Step frame _) = frame

combine :: [Sequence] -> Sequence
combine [] = V.empty
combine (seq':seqs) = seq' V.++ (combine (map (V.map (bumpFrame ((getFrame (seq' V.! 0)) + 1))) seqs))

type Colour = (Int, Int, Int)

module Childes where

import           Data.List
import qualified Data.Text     as T
import qualified Text.XML      as XML
import           Text.XML.Lens

------------------------------------------------------------------------------
-- We want:
--   1. Number of tokens per minute
--   2. Number of types per minute
--   3. Duration of utterance
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- 1 - Tokens per minute {{{

tokensPerSec :: Speaker -> [Utterance] -> [Float]
tokensPerSec speaker allUtterances = map eachAnswer relevantUtterances
  where
  eachAnswer :: Utterance -> Float
  eachAnswer utt = fromIntegral (tokens utt) / duration utt

  tokens :: Utterance -> Int
  tokens utt = length (content utt)

  duration :: Utterance -> Float
  duration utt = endTime utt - startTime utt

  relevantUtterances :: [Utterance]
  relevantUtterances = filter (\x -> utteranceSpeaker x == speaker)
                              allUtterances


---------------------------------------------------------------------------}}}
------------------------------------------------------------------------------
-- 2 - Types per minute {{{

---------------------------------------------------------------------------}}}
------------------------------------------------------------------------------
-- 3 - Duration of utterance {{{

---------------------------------------------------------------------------}}}
------------------------------------------------------------------------------
-- Helpers {{{

numberOfTokens :: String -> Int
numberOfTokens corpus = length (words corpus)

numberOfTypes :: String -> Int
numberOfTypes corpus = length (nub (words corpus))

data Speaker = MOT | CHI | FAT
  deriving (Eq, Show, Read)

data Utterance = Utterance
  { startTime        :: Float
  , endTime          :: Float
  , content          :: [T.Text]
  , utteranceSpeaker :: Speaker
  } deriving (Eq, Show, Read)

duration :: Utterance -> Float
duration u = endTime u - startTime u

sampleFile :: IO Document
sampleFile = XML.readFile XML.def "Providence/Naima/nai01.xml"

average :: [Float] -> Float
average x = sum x / fromIntegral (length x)

parseTranscript :: Document -> [Either String Utterance]
parseTranscript doc = doc ^.. root . entire . named "u" . to parseUtterance
  where
    p x = case parseUtterance x of
      Left e -> error $ "Parsing utterance:\n" ++ show x ++ "\n\nIN:\n\n" ++ e
      Right v -> v

parseUtterance :: Element -> Either String Utterance
parseUtterance s = do
  start <- comm "start" ( s ^. entire . named "media" . attribute "start")
  e <- comm "end" ( s ^. entire . named "media" . attribute "end")
  let w = (s ^.. entire . named "w" . text)
  who <- comm "who" (s ^. attribute "who")
  return $ Utterance (r start) (r e) (w) (r who)
  where
    r :: Read a => T.Text -> a
    r = read . T.unpack
    comm :: String -> Maybe v -> Either String v
    comm s Nothing = Left s
    comm _ (Just v)  = Right v


newtype Transcript = Transcript { getTranscript ::  String }
  deriving (Eq, Show, Read)
---------------------------------------------------------------------------}}}

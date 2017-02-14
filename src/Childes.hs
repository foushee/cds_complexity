{-# LANGUAGE DeriveAnyClass #-}
module Childes where

import           Control.Monad
import           Data.Csv
import           Data.Either
import           Data.List
import qualified Data.Text     as T
import           GHC.Generics  (Generic)
import           GHC.Stack
import qualified Text.XML      as XML
import           Text.XML.Lens

------------------------------------------------------------------------------

allStats :: FilePath -> IO AllStats
allStats fp = do
  doc <- XML.readFile XML.def fp
  let (bad, good) = partitionEithers $ parseTranscript doc
  return $ AllStats
    { durationStats = makeStats good UtteranceDuration
    , tokenStats = makeStats good TokensPerSec
    , typeStats = makeStats good TypesPerSec
    , ignoredUtterances = length bad
    }


makeStats :: [Utterance] -> StatType -> [Stats]
makeStats us s = case s of
  TokensPerSec -> (\s -> Stats s (average $ tokensPerSec s us)) <$> allSpeakers
  TypesPerSec -> (\s -> Stats s (average $ typesPerSec s us)) <$> allSpeakers
  UtteranceDuration -> (\s -> Stats s (average $ durations s us)) <$> allSpeakers
  where
    allSpeakers = [minBound..maxBound]
------------------------------------------------------------------------------
-- 1 - Tokens per minute {{{

tokensPerSec :: Speaker -> [Utterance] -> [Float]
tokensPerSec speaker allUtterances = map eachAnswer relevantUtterances
  where
  eachAnswer :: Utterance -> Float
  eachAnswer utt = fromIntegral (tokens utt) / duration utt

  tokens :: Utterance -> Int
  tokens utt = length (content utt)

  relevantUtterances :: [Utterance]
  relevantUtterances = filter (\x -> utteranceSpeaker x == speaker)
                              allUtterances


---------------------------------------------------------------------------}}}
------------------------------------------------------------------------------
-- 2 - Types per minute {{{

typesPerSec :: Speaker -> [Utterance] -> [Float]
typesPerSec speaker allUtterances = map eachAnswer relevantUtterances
  where
  eachAnswer :: Utterance -> Float
  eachAnswer utt = fromIntegral (types utt) / duration utt

  types :: Utterance -> Int
  types utt = length (nub $ content utt)

  relevantUtterances :: [Utterance]
  relevantUtterances = filter (\x -> utteranceSpeaker x == speaker)
                              allUtterances
---------------------------------------------------------------------------}}}
------------------------------------------------------------------------------
-- 3 - Duration of utterance {{{

durations :: Speaker -> [Utterance] -> [Float]
durations speaker allUtterances = map duration relevantUtterances
  where
  relevantUtterances :: [Utterance]
  relevantUtterances = filter (\x -> utteranceSpeaker x == speaker)
                              allUtterances
---------------------------------------------------------------------------}}}
------------------------------------------------------------------------------
-- Helpers {{{

data Speaker = MOT | CHI | FAT | GRA | ADU | AD1 | AD2 | TOY | ENV | GR1 | GR2
  deriving (Eq, Show, Enum, Read, Bounded)
instance ToField Speaker

data StatType = TokensPerSec | TypesPerSec | UtteranceDuration
  deriving (Eq, Show, Enum, Read, Bounded)

data Utterance = Utterance
  { startTime        :: Float
  , endTime          :: Float
  , content          :: [T.Text]
  , utteranceSpeaker :: Speaker
  } deriving (Eq, Show, Read)

data Stats = Stats
  { statsSpeaker :: Speaker
  , statsMean    :: Float
  } deriving (Eq, Show, Read, Generic)
instance ToRecord Stats

data AllStats = AllStats
  { durationStats     :: [Stats]
  , tokenStats        :: [Stats]
  , typeStats         :: [Stats]
  , ignoredUtterances :: Int
  } deriving (Eq, Show, Read, Generic)

prettyIsh :: AllStats -> IO ()
prettyIsh a = do
  putStrLn "Duration:"
  forM_ (durationStats a) $ \s -> do
    putStrLn $ "Speaker:\t" ++ (show $ statsSpeaker s)
    putStrLn $ "Mean:\t" ++ (show $ statsMean s)

  putStrLn "Token:"
  forM_ (tokenStats a) $ \s -> do
    putStrLn $ "Speaker:\t" ++ (show $ statsSpeaker s)
    putStrLn $ "Mean:\t" ++ (show $ statsMean s)

  putStrLn "Type:"
  forM_ (typeStats a) $ \s -> do
    putStrLn $ "Speaker:\t" ++ (show $ statsSpeaker s)
    putStrLn $ "Mean:\t" ++ (show $ statsMean s)

  putStrLn $ "Ignored " ++ show (ignoredUtterances a) ++ " utterances"

duration :: Utterance -> Float
duration u = endTime u - startTime u

sampleFile :: IO Document
sampleFile = XML.readFile XML.def "Providence/Naima/nai54.xml"

average :: [Float] -> Float
average x = sum x / fromIntegral (length x)

parseTranscript :: Document -> [Either String Utterance]
parseTranscript doc = doc ^.. root . entire . named "u" . to parseUtterance

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

---------------------------------------------------------------------------}}}

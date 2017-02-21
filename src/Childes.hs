{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Childes where

import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import           Data.List
import           Data.Monoid
import           Data.String          (IsString (..))
import qualified Data.Text            as T
import           Data.Time            (defaultTimeLocale, diffUTCTime,
                                       parseTimeOrError)
import           GHC.Generics         (Generic)
import           GHC.Stack            (HasCallStack)
import qualified Text.XML             as XML
import           Text.XML.Lens


newtype Speaker = Speaker { getSpeaker :: T.Text }
  deriving (Eq, Show, Read, IsString)

instance ToField Speaker where
  toField = toField . getSpeaker

data Transcript = Transcript
  { tcCorpus     :: T.Text
  , tcChild      :: T.Text
  , tcChildAge   :: Float
  , tcUtterances :: [Either Error Utterance]
  , tcFile       :: FilePath
  } deriving (Eq, Show, Read, Generic)

toCSV :: [Transcript] -> BS.ByteString
toCSV ts = encodeDefaultOrderedByName . concat $ transcriptToRows <$> ts

data Utterance = Utterance
  { startTime        :: Float
  , endTime          :: Float
  , content          :: [T.Text]
  , utteranceSpeaker :: Speaker
  } deriving (Eq, Show, Read, Generic)

data Error = NoMediaTag Speaker | MissingSpeaker | UnknownError
  deriving (Eq, Show, Read, Generic)

instance ToField Error where
  toField (NoMediaTag e) = toField $ "No media tag for speaker:" <> getSpeaker e
  toField MissingSpeaker = "Speaker missing"
  toField UnknownError = "Unknown error"

transcriptToRows :: Transcript -> [Row]
transcriptToRows ts = rows
  where
    rows = fmap (uncurry $ utteranceToRow ts) ixUtts
    ixUtts = zip [1..] $ tcUtterances ts

utteranceToRow :: Transcript -> Int -> Either Error Utterance -> Row
utteranceToRow ts no (Left e) = Row
  { study_id = "19.4"
  , input = "N/A"
  , corpus = tcCorpus ts
  , child = tcChild ts
  , child_age_mos = tcChildAge ts
  , file = tcFile ts
  , row_number = no
  , speaker = Nothing
  , duration_secs = Nothing
  , tokens = Nothing
  , types = Nothing
  , error_type = Just e
  }
utteranceToRow ts no (Right utt) = Row
  { study_id = "19.4"
  , input = "N/A"
  , corpus = tcCorpus ts
  , child = tcChild ts
  , child_age_mos = tcChildAge ts
  , file = tcFile ts
  , row_number = no
  , speaker = Just $ utteranceSpeaker utt
  , duration_secs = Just $ duration utt
  , tokens = Just $ length (content utt)
  , types = Just $ length (nub $ content utt)
  , error_type = Nothing
  }

data Row = Row
  { study_id      :: T.Text
  , input         :: T.Text
  , corpus        :: T.Text
  , child         :: T.Text
  , child_age_mos :: Float
  , file          :: FilePath
  , row_number    :: Int
  , speaker       :: Maybe Speaker
  , duration_secs :: Maybe Float
  , tokens        :: Maybe Int
  , types         :: Maybe Int
  , error_type    :: Maybe Error
  } deriving (Eq, Show, Read, Generic)

instance ToRecord Row
instance ToNamedRecord Row
instance DefaultOrdered Row

duration :: Utterance -> Float
duration u = endTime u - startTime u

{-sampleFile :: IO Document-}
{-sampleFile = XML.readFile XML.def "Providence/Naima/nai54.xml"-}

average :: [Float] -> Float
average x = sum x / fromIntegral (length x)

parseTranscript :: FilePath -> IO Transcript
parseTranscript fp = parseTranscript' fp <$> XML.readFile XML.def fp

parseTranscript' :: HasCallStack => FilePath -> Document -> Transcript
parseTranscript' fp doc = Transcript
  { tcCorpus = doc ^. root . attr "Corpus"
  , tcChild = doc ^. root ./ named "Participants" ./ attributeIs "id" "CHI" . attr "name"
  , tcChildAge = age
  , tcUtterances = doc ^.. root . entire . named "u" . to parseUtterance
  , tcFile = fp
  }
  where
    parseTime x y = parseTimeOrError True defaultTimeLocale x (T.unpack y)
    date = parseTime "%F" $ doc ^. root . attr "Date"
    birth = parseTime "%F"
          $ doc ^. root ./ named "Participants"
                        ./ attributeIs "id" "CHI"
                        . attr "birthday"
    age = (fromRational $ toRational $ diffUTCTime date birth) / (60 * 60 * 24 * 30)

parseUtterance :: Element -> Either Error Utterance
parseUtterance s = do
  who <- Speaker <$> note MissingSpeaker (s ^. attribute "who")
  start <- note (NoMediaTag who) ( s ^. entire . named "media" . attribute "start")
  e <- note (NoMediaTag who) ( s ^. entire . named "media" . attribute "end")
  let w = (s ^.. entire . named "w" . text)
  return $ Utterance (r start) (r e) (w) who
  where
    r :: Read a => T.Text -> a
    r = read . T.unpack
    note :: Error -> Maybe v -> Either Error v
    note e = maybe (Left e) Right

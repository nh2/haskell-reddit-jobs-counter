{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where


import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Foldable (for_, toList)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           GHC.Generics (Generic)
import qualified Network.Reddit as R
import           Network.Reddit as R
import           Network.Reddit.Types.Internal as R
import           Network.Reddit.Types.Award as R
import           Network.Reddit.Types.Item as R
import           System.FilePath ((</>))
import           System.Directory (createDirectoryIfMissing)


-- Instances to be able to save `SimpleSubmission` as JSON.
-- Unfortunately we couldn't do it directly with `Submission`
-- because `heddit` provides only a `FromJSON` instance which
-- does not agree with a generically derived `ToJSON` instance.
instance ToJSON SubmissionID
instance ToJSON SubmissionContent
instance ToJSON ItemReport
instance ToJSON Distinction
instance ToJSON PollOption
instance ToJSON PollData
instance ToJSON AwardID
instance ToJSON SubredditID
instance ToJSON AwardType
instance ToJSON Awarding
instance ToJSON Submission

instance FromJSON SubmissionContent


-- | Like `Submission` but containing only fields we care about.
-- Having this allows us to serialise it so we can scrape it only once.
data SimpleSubmission = SimpleSubmission
  { submissionID  :: SubmissionID
  , title         :: Title
  , author        :: Username
  , content       :: SubmissionContent
  , created       :: UTCTime
  , permalink     :: URL
  , numComments   :: Integer
  , score         :: Integer
  , ups           :: Maybe Integer
  , downs         :: Maybe Integer
  , upvoteRatio   :: Maybe Rational
  , gilded        :: Integer
  }
  deriving (Show, Eq, Generic)
instance ToJSON SimpleSubmission
instance FromJSON SimpleSubmission


storageDir :: FilePath
storageDir = "title_searches"

storeSubmission :: (MonadIO m) => SimpleSubmission -> m ()
storeSubmission s = do
  let fileName = case Main.submissionID s of R.SubmissionID x -> T.unpack x
  liftIO $ Aeson.encodeFile (storageDir </> (fileName ++ ".json")) s


main :: IO ()
main = do
  -- Requires config file `~/.config/heddit/auth.ini` as explained in the `heddit` README.
  client <- loadClient (Just "haskell-job-scraper")
  subname <- mkSubredditName "haskell"

  let storeSearchResults :: Text -> RedditT IO ()
      storeSearchResults searchTerm = do

        liftIO $ createDirectoryIfMissing False storageDir

        let processListing listing = do
              for_ (children listing) $ \submission -> do
                liftIO $ print (R.submissionID submission, case submission of R.Submission{ created } -> created)
                let simpleSubmission = SimpleSubmission{..} where Submission{..} = submission
                storeSubmission simpleSubmission

        let storageLoop !paginator !numTotal = do
              liftIO $ print paginator

              -- Going backwards in time:
              --     listing <- getNewSubmissions subname paginator
              -- But Reddit results allow only <= 1000 results so we need to use the search
              -- feature to find all Main.
              listing <- search R.Search{ q = searchTerm, subreddit = Just subname, syntax = Nothing } paginator

              let !numSubmissions = length (children listing)
              when (numSubmissions > 0) $ do
                let !newTotal = numTotal + numSubmissions
                liftIO $ putStrLn $ "Got " ++ show numSubmissions ++ " submissions, total: " ++ show newTotal
                processListing listing
                liftIO $ threadDelay 1010000 -- 1.01 seconds to have less than 60 requests per second as per API guidelines
                let !nextPaginator = nextPage (Just paginator) listing
                if (case nextPaginator of Paginator{ after } -> isJust after) then do
                  storageLoop nextPaginator newTotal
                else do
                  liftIO $ putStrLn $ "Total submissions for search term " ++ T.unpack searchTerm ++ ": " ++ show newTotal

        storageLoop (emptyPaginator{ limit = 100 }) 0
  runReddit client $ do


    let searchTerms =
          [ "flair:job"
          , "title:engineer"
          , "title:hiring"
          , "title:job"
          , "title:position" -- oddly this finds exactly as many results as "job", probably Reddit maps these words together
          , "title:role"
          , "title:vacancy"
          ]
    for_ searchTerms storeSearchResults

    {-
    Post-processing in the shell:

    -- Replace newlines and tabs
    jq --raw-output "\"\(.created)\t\( .title | split(\"\n\") | join(\" \") | split(\"\t\") | join (\" \") )\"" title_searches/* | sort

    -- Better output for Google Sheets, which does not support ISO-8601 (https://stackoverflow.com/questions/30492832/iso-8601-string-to-date-in-google-sheets-cell):
    jq --raw-output "\"\(.created | strptime(\"%Y-%m-%dT%H:%M:%SZ\") | strftime(\"%Y-%m-%d %H:%M:%S\") )\t\( .title | split(\"\n\") | join(\" \") | split(\"\t\") | join (\" \") )\"" title_searches/* | sort

    -- Per-year counts:
    for YEAR in {2008..2022}; do echo -n "$YEAR "; jq --raw-output "\"\(.created | .[:7]) \( .title | split(\"\n\") | join(\"\") )\"" title_searches/* | sort | grep "${YEAR}-" | wc -l; done
    -}

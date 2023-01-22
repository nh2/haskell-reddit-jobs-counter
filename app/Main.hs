{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Foldable (for_)
import Network.Reddit
-- import Network.Reddit.Types.Internal (Paginator(..), children, limit)
import Network.Reddit.Types.Internal (children, limit, after)
-- import Network.Reddit.Types.Internal (children, limit, Paginator(after))
import Network.Reddit.Types.Submission (q, Search(..), syntax, Submission(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath

import qualified Data.Text as T
import Network.Reddit.Types.Award
import Data.Time
import GHC.Generics

instance ToJSON AwardID
instance ToJSON SubredditID
instance ToJSON AwardType
instance ToJSON SubmissionID
instance ToJSON SubmissionContent
instance ToJSON PollOption
instance ToJSON ItemReport
instance ToJSON Distinction
instance ToJSON PollData
instance ToJSON Awarding
instance ToJSON Submission

-- | Like `Submission` but containing only fields we care about.
-- Having this allows us to serialise it so we can scrape it only once.
-- data SimpleSubmission = SimpleSubmission
--   { submissionID  :: SubmissionID
--   , title         :: Title
--   , author        :: Username
--   , content       :: SubmissionContent
--   , created       :: UTCTime
--   , permalink     :: URL
--   , numComments   :: Integer
--   , score         :: Integer
--   , ups           :: Maybe Integer
--   , downs         :: Maybe Integer
--   , upvoteRatio   :: Maybe Rational
--   , gilded        :: Integer
--   }
--   deriving (Show, Eq, Generic)
-- instance ToJSON SimpleSubmission

searchDir :: FilePath
searchDir = "job_searches"

main :: IO ()
main = do
  client <- loadClient (Just "haskell-job-scraper")
  subName <- mkSubredditName "haskell"


  runReddit client $ do
    let processListing l = liftIO $ do
          -- for_ (children l) $ \s@Submission{..} -> do
          for_ (children l) $ \(s :: Submission) -> do
            print (title s)
            -- let fileName = case submissionID of SubmissionID x -> T.unpack x
            -- let simpleSubmission = SimpleSubmission{..} where Submission{..} = s
            -- encodeFile (searchDir </> (fileName ++ ".json")) simpleSubmission
            return ()


    let searchResultsFor searchTerm = do

          let loopPages paginator numTotal = do
                -- listing <- getNewSubmissions subName paginator
                listing <- search Search{ q = searchTerm, subreddit = Just subName, syntax = Nothing } paginator
                let numSubmissions = length (children listing)
                let newTotal = numTotal + numSubmissions
                liftIO $ putStrLn $ "Total submissions so far: " ++ show newTotal
                processListing listing
                when (numSubmissions > 0) $ do
                  let nextPaginator = nextPage (Just paginator) listing
                  case nextPaginator of
                    Paginator{ .. } -> case after of
                      Nothing -> liftIO $ putStrLn $ "Finished crawling, total submissions for term " ++ T.unpack searchTerm ++ ": " ++ show newTotal
                      _ -> loopPages nextPaginator newTotal

          loopPages emptyPaginator{ limit = 100 } 0

    liftIO $ createDirectoryIfMissing False searchDir

    let searchTerms =
          [ "flair:job"
          , "title:job"
          , "title:hiring"
          , "title:developer"
          , "title:engineer"
          , "title:vacancy"
          , "title:role"
          , "title:position"
          , "title:senior"
          , "title:junior"
          ]
    for_ searchTerms searchResultsFor


  -- Use client to make requests
  -- Get /new submissions backwards in time:
  -- Write a loop to paginate
  -- For each post:
  --   save it as [postID].json
  -- Do analysis and graphing

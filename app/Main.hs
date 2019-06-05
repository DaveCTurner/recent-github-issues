{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Network.Wreq
import System.Environment
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import Control.Monad
import Text.Printf
import Data.Time

data UserResponseBody = UserResponseBody
  { _urLogin :: String
  } deriving (Show, Eq)

instance FromJSON UserResponseBody where
  parseJSON = withObject "UserResponseBody" $ \v -> UserResponseBody
    <$> v .: "login"

data SearchResponseBody = SearchResponseBody
  { _srIssues :: [Issue]
  } deriving (Show, Eq)

instance FromJSON SearchResponseBody where
  parseJSON = withObject "SearchResponseBody" $ \v -> SearchResponseBody
    <$> v .: "items"

data Issue = Issue
  { _issueHtmlUrl    :: T.Text
  , _issueTitle      :: T.Text
  , _issueNumber     :: Integer
  , _issueRepository :: T.Text
  , _issueUpdated    :: UTCTime
  } deriving (Show, Eq)

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \v -> Issue
    <$> v .: "html_url"
    <*> v .: "title"
    <*> v .: "number"
    <*> v .: "repository_url"
    <*> v .: "updated_at"

issueRepoAbbreviation :: Issue -> String
issueRepoAbbreviation i = if
  | repo == "https://api.github.com/repos/elastic/elasticsearch"                  -> ""
  | repo == "https://api.github.com/repos/elastic/support-dev-help"               -> "sdh"
  | repo == "https://api.github.com/repos/elastic/support-dev-help-elasticsearch" -> "sdhe"
  | "https://api.github.com/repos/elastic/" `T.isPrefixOf` repo                   -> T.unpack $ T.drop (T.length "https://api.github.com/repos/")         repo
  | "https://api.github.com/repos/"         `T.isPrefixOf` repo                   -> T.unpack $ T.drop (T.length "https://api.github.com/repos/elastic/") repo
  | otherwise                                                                     -> error $ "unknown repo format: " ++ show repo

  where
    repo = _issueRepository i

main :: IO ()
main = do
  token <- getEnv "GITHUB_ISSUES_TOKEN"

  let opts = defaults 
        & header "Accept"        .~ ["application/vnd.github.v3+json"]
        & header "Authorization" .~ [B.append "token " $ T.encodeUtf8 $ T.pack token]

  userResponse <- asJSON =<< getWith opts "https://api.github.com/user"
  let userResponseBody = userResponse ^. responseBody

  startDay <- show . addDays (-8) . utctDay <$> getCurrentTime

  let go url = do
        searchResponse <- asJSON =<< getWith opts url
        forM_ (_srIssues $ searchResponse ^. responseBody) $ \issue ->
          putStrLn $ printf "[%s] ([%s#%d](%s)) %s"
            (show $ utctDay $ _issueUpdated issue)
            (issueRepoAbbreviation    issue)
            (_issueNumber             issue)
            (T.unpack $ _issueHtmlUrl issue)
            (T.unpack $ _issueTitle   issue)
        case searchResponse ^? responseLink "rel" "next" . linkURL of
          Just url' -> go $ T.unpack $ T.decodeUtf8 url'
          Nothing   -> return ()

  go $ "https://api.github.com/search/issues?q=involves:" ++ _urLogin userResponseBody ++ "+updated:>=" ++ startDay ++ "&sort=updated"



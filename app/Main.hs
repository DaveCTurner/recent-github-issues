{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Network.Wreq
import System.Environment
import Control.Lens
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import Control.Monad
import Text.Printf
import Data.Time
import System.IO
import System.FilePath
import System.Directory

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
  | repo == "https://api.github.com/repos/elastic/support-support-help"           -> "ssh"
  | repo == "https://api.github.com/repos/elastic/support-dev-help"               -> "sdh"
  | repo == "https://api.github.com/repos/elastic/sdh-elasticsearch"              -> "sdhe"
  | repo == "https://api.github.com/repos/elastic/cloud-on-k8s"                   -> "eck"
  | repo == "https://api.github.com/repos/elastic/elasticsearch-team-planning"    -> "es-plans"
  | repo == "https://api.github.com/repos/elastic/elasticsearch-serverless"       -> "es-serverless"
  | "https://api.github.com/repos/elastic/" `T.isPrefixOf` repo                   -> T.unpack $ T.drop (T.length "https://api.github.com/repos/elastic/") repo
  | "https://api.github.com/repos/"         `T.isPrefixOf` repo                   -> T.unpack $ T.drop (T.length "https://api.github.com/repos/")         repo
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

  today <- utctDay <$> getCurrentTime
  let startDay = show $ addDays (-16) today

  homeDir <- getEnv "HOME"
  let outputFile = homeDir </> "status-updates" </> (show today ++ ".mm")
  exists <- doesFileExist outputFile
  when exists $ error $ outputFile ++ " already exists"

  putStrLn $ "creating " ++ outputFile
  withFile outputFile WriteMode $ \h -> do

    let go pageNum url = do
          when (pageNum > 10) $ threadDelay 1000000
          when (pageNum > 15) $ threadDelay 5000000
          searchResponse <- asJSON =<< getWith opts url
          forM_ (_srIssues $ searchResponse ^. responseBody) $ \issue ->
            hPutStrLn h $ printf "<node TEXT='[%s] ([%s#%d](%s)) %s' LINK='%s' MAX_WIDTH='100 cm' />"
              (show $ utctDay $ _issueUpdated issue)
              (issueRepoAbbreviation    issue)
              (_issueNumber             issue)
              (T.unpack $ _issueHtmlUrl issue)
              (concatMap (\c -> case c of
                  '&'  -> "&amp;"
                  '\'' -> "&apos;"
                  '<'  -> "&lt;"
                  '>'  -> "&gt;"
                  otherwise -> [c])
              $T.unpack $ _issueTitle   issue)
              (T.unpack $ _issueHtmlUrl issue)
          case searchResponse ^? responseLink "rel" "next" . linkURL of
            Just url' -> go (pageNum + 1) $ T.unpack $ T.decodeUtf8 url'
            Nothing   -> return ()

    hPutStrLn h   "<?xml version='1.0' encoding='UTF-8'?>"
    hPutStrLn h   "<map version='freeplane 1.9.0'>"
    hPutStrLn h $ "<node TEXT='" ++ show today ++ "&#xA;Status update' STYLE='oval' FOLDED='false'>"
    hPutStrLn h   "<node TEXT='issues' POSITION='right'>"

    go 0 $ "https://api.github.com/search/issues?per_page=100&q=involves:" ++ _urLogin userResponseBody ++ "+updated:>=" ++ startDay ++ "&sort=updated"

    hPutStrLn h   "</node></node></map>"


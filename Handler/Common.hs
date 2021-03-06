-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import Data.Time.Clock()
import Data.Time.Calendar()
import Network.Wai
import qualified Data.Foldable as F

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
                    
isEmpty :: F.Foldable t => t a -> Bool
isEmpty = F.foldr (\_ _ -> False) True

getDate :: IO (Integer, Int, Int)
getDate = getCurrentTime >>= return . toGregorian . utctDay

printDate :: (Integer, Int, Int) -> Text
printDate (year, month, day) =  
          pack ((show year) ++ "-" ++ (show month) ++ "-" ++ (show day)) 

getDateStr :: IO (Text)
getDateStr = do
    date <- getDate
    let converted = printDate date
    return converted

getIpFromHeader = do
    ip1 <- lookupHeader "X-Real-IP"
    ip2 <- lookupHeader "X-Forwarded-For" 
    case ip1 of 
                      Just ip -> return ip
                      Nothing -> case ip2 of
                                      Just ip -> return ip
                                      Nothing -> error "Cannot get client IP"

getIp = do
    ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
    return $ pack $ takeWhile (/=':') ip
 
validateVoterIp rId = do
    today <- liftIO $ getDateStr
    ip <- getIp
    invalidRecords <- runDB $ selectList [VotesDate ==. today, VotesIp ==. ip, VotesRId ==. rId] []
    return $ isEmpty $ invalidRecords
 
insertVoterIp rId = do
    ip <- getIp
    today <- lift $ liftIO getDateStr
    let record = Votes ip rId today
    _ <- runDB $ insert record
    return ()

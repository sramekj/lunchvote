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

getIP = do
    ip1 <- lookupHeader "X-Real-IP"
    ip2 <- lookupHeader "X-Forwarded-For" 
    case ip1 of 
                      Just ip -> return ip
                      Nothing -> case ip2 of
                                      Just ip -> return ip
                                      Nothing -> error "Cannot get client IP"

getIP2 = do
    ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
    return $ pack $ takeWhile (/=':') ip
 
validateVoterIp = do
    today <- liftIO $ getDateStr
    ip <- getIP2
    invalidRecords <- runDB $ selectList [VotesDate ==. today, VotesIp ==. ip] []
    return $ isEmpty $ invalidRecords
 
insertVoterIp = do
    ip <- getIP2
    today <- lift $ liftIO getDateStr
    let record = Votes ip today
    _ <- runDB $ upsert record [VotesDate =. today]
    return ()


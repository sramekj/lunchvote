module Handler.ListMeals where

import Import
import Handler.Cache
import Handler.Pool
import qualified Data.Foldable as F

getIP = do
    ip1 <- lookupHeader "X-Real-IP"
    ip2 <- lookupHeader "X-Forwarded-For" 
    case ip1 of 
                      Just ip -> return ip
                      Nothing -> case ip2 of
                                      Just ip -> return ip
                                      Nothing -> error "Cannot get client IP"

isEmpty :: F.Foldable t => t a -> Bool
isEmpty = F.foldr (\_ _ -> False) True

validateVoterIp = do
    today <- liftIO $ getDateStr
    ip <- getIP
    invalidRecords <- runDB $ selectList [VotesDate ==. today, VotesIp ==. ip] []
    return $ isEmpty $ invalidRecords
 
insertVoterIp = do
    ip <- getIP
    today <- lift $ liftIO getDateStr
    let record = Votes ip today
    _ <- runDB $ upsert record [VotesDate =. today]
    return ()

getListMealsR :: Handler Html
getListMealsR = do
    menuList <- lift $ getData 
    canVote <- validateVoterIp
--  if canVote then insertVoterIp
--             else $(logInfo) "User cannot vote"

    


  --   case lift $ canVote of True -> return ()
  --                          False -> return ()

--      let keke = getData'
--      ids <- liftIO getRestaurantIds
----      print ids
--        print <$> ids

--        json <- getJSON resKey
--        liftIO $ print json

        
--     clientIp <- getIP
--     liftIO $ print ("Client IP: " ++ clientIp)
    defaultLayout $ do
        $(widgetFile "list")


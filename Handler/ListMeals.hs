module Handler.ListMeals where

import Import
import Handler.Common
import Handler.Cache

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


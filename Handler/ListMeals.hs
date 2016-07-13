module Handler.ListMeals where

import Import
import Handler.Cache

getIP = do
    ip1 <- lookupHeader "X-Real-IP"
    ip2 <- lookupHeader "X-Forwarded-For" 
    case ip1 of 
                      Just ip -> return ip
                      Nothing -> case ip2 of
                                      Just ip -> return ip
                                      Nothing -> error "Cannot get client IP"



getListMealsR :: Handler Html
getListMealsR = do
    defaultLayout $ do
        menuList <- lift $ getData 
        
--        let keke = getData'
--      ids <- liftIO getRestaurantIds
----      print ids
--        print <$> ids

--        json <- getJSON resKey
--        liftIO $ print json

        
        clientIp <- getIP
        liftIO $ print ("Client IP: " ++ clientIp)
        $(widgetFile "list")


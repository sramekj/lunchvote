module Handler.ListMeals where

import Import
import Handler.Common
import Handler.Cache

getListMealsR :: Handler Html
getListMealsR = do
    -- ip <- getIp 
    -- putStrLn $ pack "Client list request from IP: " ++ ip
    menuList <- lift $ getData 
    canVote <- validateVoterIp
    defaultLayout $ do
        $(widgetFile "list")


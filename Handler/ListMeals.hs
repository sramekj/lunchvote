module Handler.ListMeals where

import Import
import Handler.Common
import Handler.Cache

getListMealsR :: Handler Html
getListMealsR = do
    -- ip <- getIp 
    -- putStrLn $ pack "Client list request from IP: " ++ ip
    menuList <- lift $ getData
    canVoteList <- filterM (\menuItem -> do
                                           canVote <- validateVoterIp $ Handler.Cache.id menuItem
                                           return canVote
                           ) menuList
    let canVoteIdList = fmap (\k -> Handler.Cache.id k) canVoteList
    defaultLayout $ do
        $(widgetFile "list")



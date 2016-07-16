module Handler.ListMeals where

import Import
import Handler.Common
import Handler.Cache

getListMealsR :: Handler Html
getListMealsR = do
    menuList <- lift $ getData 
    canVote <- validateVoterIp
    defaultLayout $ do
        $(widgetFile "list")


module Handler.ListMeals where

import Import
import Handler.Cache

getListMealsR :: Handler Html
getListMealsR = do
    defaultLayout $ do
        let menuList = getData 
        $(widgetFile "list")

module Handler.ListMeals where

import Import

getListMealsR :: Handler Html
getListMealsR = do
    defaultLayout $ do
        $(widgetFile "list")

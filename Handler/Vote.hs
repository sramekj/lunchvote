module Handler.Vote where

import Import
import Handler.Cache

postVoteR :: Int -> Handler Html
postVoteR restaurantId = do
    redirect $ PoolR

getVoteR :: Int -> Handler Html
getVoteR restaurantId = do
    defaultLayout [whamlet|<h1>#{restaurantId}|]


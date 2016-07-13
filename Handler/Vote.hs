module Handler.Vote where

import Import
import Handler.Pool

postVoteR :: Int -> Handler Html
postVoteR restaurantId = do
    _ <- validateDb
    runDB $ updateWhere [PoolRestaurantId ==. restaurantId] [PoolVotes +=. 1]
    defaultLayout [whamlet||]

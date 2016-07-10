module Handler.Vote where

import Import
import Handler.Cache
import qualified Network.HTTP.Types as H

postVoteR :: Int -> Handler Html
postVoteR restaurantId = do
    runDB $ updateWhere [PoolRestaurantId ==. restaurantId] [PoolVotes +=. 1]
    defaultLayout [whamlet||]

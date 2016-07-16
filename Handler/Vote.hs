module Handler.Vote where

import Import
import Handler.Common
import Handler.Pool

postVoteR :: Int -> Handler Html
postVoteR restaurantId = do
    prepareDb
    canVote <- validateVoterIp
    case canVote of 
            True -> do
                runDB $ updateWhere [PoolRestaurantId ==. restaurantId] [PoolVotes +=. 1]
                insertVoterIp
            False -> return ()
    defaultLayout [whamlet||]

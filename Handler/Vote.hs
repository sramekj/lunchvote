module Handler.Vote where

import Import
import Handler.Common
import Handler.Poll

postVoteR :: Int -> Handler Html
postVoteR restaurantId = do
    prepareDb
    canVote <- validateVoterIp restaurantId
    case canVote of 
            True -> do
                runDB $ updateWhere [PollRestaurantId ==. restaurantId] [PollVotes +=. 1]
                insertVoterIp restaurantId
            False -> return ()
    defaultLayout [whamlet||]

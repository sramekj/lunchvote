module Handler.Vote where

import Import
import Handler.Cache

postVoteR :: Int -> Handler Html
postVoteR restaurantId = do
    defaultLayout [whamlet||]

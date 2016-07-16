module Handler.Purge where

import Import

purge = runDB $ do
    deleteWhere ([] :: [Filter Pool])
    deleteWhere ([] :: [Filter Votes])
 
getPurgeR :: Handler Html
getPurgeR = do
    purge
    defaultLayout $ do
        [whamlet|<h1>DB purged!|]

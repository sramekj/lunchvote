module Handler.Purge where

import Import
import Handler.Common

purge = runDB $ do
    deleteWhere ([] :: [Filter Poll])
    deleteWhere ([] :: [Filter Votes])
 
compareIp myIp = do
    ip <- getIP
    return $ ip == myIp

getPurgeR :: Handler Html
getPurgeR = do
    approval <- compareIp "10.1.2.237"
    case approval of 
            True -> do
                purge
                defaultLayout $ do
                    [whamlet|<div .jumbotron .text-center><h1>DB purged!|]
            False -> do
                defaultLayout $ do
                    [whamlet|<div .jumbotron .text-center><h1>Nice try!|]

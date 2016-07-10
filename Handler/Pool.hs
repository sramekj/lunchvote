module Handler.Pool where

import Import
import Data.List (sortBy)
import Data.Ord (comparing)

data VoteRecord = VoteRecord 
                    {
                        id :: Int,
                        restaurant :: Text,
                        votes :: Int
                    }

type VoteRecords = [VoteRecord]

getDummyVotes :: VoteRecords
getDummyVotes = [VoteRecord{id = 1, restaurant = "PotrefenaHusa", votes = 5},VoteRecord{id = 2, restaurant = "Coolna", votes = 2},VoteRecord{id = 3, restaurant = "Harryho restaurant", votes = 7}]

getPoolR :: Handler Html
getPoolR = do
    defaultLayout $ do
        let results = reverse . Data.List.sortBy (comparing votes) $ getDummyVotes
        $(widgetFile "pool")

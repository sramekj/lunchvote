module Handler.Pool where

import Import
import qualified Data.Map as M
import Data.List (sortBy)
import qualified Data.List as L
import Data.Ord (comparing)
import Handler.Cache

data VoteResult = VoteResult
                    { name :: Text,
                      votes :: Int
                    }

getRestaurantName :: Int -> [(Int,Text)] -> Text
getRestaurantName index restaurantList = let restaurantItem = L.find (\k -> fst k == index) restaurantList
                                         in case restaurantItem of
                                                Just (_, x) -> x
                                                otherwise -> error "Invalid key"

getVoteData :: VoteRecords -> MenuList -> [VoteResult]
getVoteData recordMap menuList = let restaurants = fmap (\k -> (Handler.Cache.id k, restaurant k)) menuList
                                     sortedVotes = reverse . L.sortBy (comparing snd) $ M.toList recordMap
                                 in (\k -> VoteResult{name = getRestaurantName (fst k) restaurants, votes = snd k}) <$> sortedVotes
                                 

getPoolR :: Handler Html
getPoolR = do
    defaultLayout $ do
        let results = getVoteData getVotes getData
        $(widgetFile "pool")

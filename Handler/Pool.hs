module Handler.Pool where

import Import
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sortBy)
import qualified Data.List as L
import Data.Ord (comparing)
import Handler.ListMeals

data VoteResult = VoteResult
                    { name :: Text,
                      votes :: Int
                    }

type VoteRecords = Map Int Int

getDummyVotes :: VoteRecords
getDummyVotes = M.fromList [(0,5), (1,2), (2,7)]

getRestaurantName :: Int -> [(Int,Text)] -> Text
getRestaurantName index restaurantList = let restaurantItem = L.find (\k -> fst k == index) restaurantList
                                         in case restaurantItem of
                                                Just (_, x) -> x
                                                otherwise -> error "Invalid key"

getVoteData :: VoteRecords -> MenuList -> [VoteResult]
getVoteData recordMap menuList = let restaurants = fmap (\k -> (Handler.ListMeals.id k, restaurant k)) menuList
                                     sortedVotes = M.toDescList recordMap
                                 in fmap (\k -> VoteResult{name = getRestaurantName (fst k) restaurants, votes = snd k}) sortedVotes
                                 

getPoolR :: Handler Html
getPoolR = do
    defaultLayout $ do
        let results = getVoteData getDummyVotes getDummyData
        $(widgetFile "pool")

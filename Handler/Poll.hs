module Handler.Poll where

import Import
import qualified Data.Map as M
import Data.List (sortBy)
import qualified Data.List as L
import Data.Ord (comparing)
import Handler.Cache
import Handler.Common

type VoteRecords = Map Int Int


data VoteResult = VoteResult
                    { name :: Text,
                      votes :: Int
                    }

getRestaurantName :: Int -> [(Int,Text)] -> Text
getRestaurantName idx restaurantList = let restaurantItem = L.find (\k -> fst k == idx) restaurantList
                                       in case restaurantItem of
                                                Just (_, x) -> x
                                                _ -> error "Invalid key"

getVoteData :: VoteRecords -> MenuList -> [VoteResult]
getVoteData recordMap menuList = let restaurants = fmap (\k -> (Handler.Cache.id k, restaurant k)) menuList
                                     sortedVotes = reverse . L.sortBy (comparing snd) $ M.toList recordMap
                                 in (\k -> VoteResult{name = getRestaurantName (fst k) restaurants, votes = snd k}) <$> sortedVotes
                                 
dbDataToVoteRecords :: [(Int, Int, Text)] -> VoteRecords
dbDataToVoteRecords = M.fromList . fmap (\(a, b, _) -> (a, b))

initDb = runDB $ do
    rdata <- liftIO $ getData
    currentDate <- lift $ liftIO getDateStr
    deleteWhere [PollDate ==. currentDate]
    _ <- forM rdata $ \k -> do
        insert $ Poll (Handler.Cache.id k)  0 currentDate
    return ()

readDb = runDB $ do
    currentDate <- lift $ liftIO getDateStr
    dbData <- selectList[PollDate ==. currentDate] [Desc PollVotes]
    return $ (\(Entity _ d)  -> (pollRestaurantId d, pollVotes d, pollDate d)) <$> dbData

prepareDb = do
    currentDate <- lift $ liftIO getDateStr
    currentDateRecords <- runDB $ selectList [PollDate ==. currentDate] []
    result <- case currentDateRecords of
                        [] -> do initDb
                        _ -> return ()     
    return result


getPollR :: Handler Html
getPollR = do
    prepareDb
    dbData <- readDb
    defaultLayout $ do
        rdata <- lift $ getData
        let results = getVoteData (dbDataToVoteRecords dbData) rdata
        $(widgetFile "poll")

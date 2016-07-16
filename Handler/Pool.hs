module Handler.Pool where

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

initDb rList = runDB $ do
        currentDate <- lift $ liftIO getDateStr
        deleteWhere [PoolDate ==. currentDate]
        _ <- forM rList $ \k -> do
            insert $ Pool (Handler.Cache.id k)  0 currentDate
        return ()

readDb = runDB $ do
    currentDate <- lift $ liftIO getDateStr
    dbData <- selectList[PoolDate ==. currentDate] [Desc PoolVotes]
    return $ (\(Entity _ d)  -> (poolRestaurantId d, poolVotes d, poolDate d)) <$> dbData

validatedReadDb = do
    currentDate <- lift $ liftIO getDateStr
    currentDateRecords <- runDB $ selectList [PoolDate ==. currentDate] []
    result <- case currentDateRecords of
                        [] -> do rdata <- lift $ getData
                                 initDb rdata
                                 readDb
                        records -> return $ (\(Entity _ d)  -> (poolRestaurantId d, poolVotes d, poolDate d)) <$> records 
    return result

prepareDb = do
    currentDate <- lift $ liftIO getDateStr
    currentDateRecords <- runDB $ selectList [PoolDate ==. currentDate] []
    result <- case currentDateRecords of
                        [] -> do rdata <- lift $ getData
                                 initDb rdata
                        _ -> return ()     
    return result


getPoolR :: Handler Html
getPoolR = do
    dbData <- validatedReadDb
    defaultLayout $ do
        rdata <- lift $ getData
        let results = getVoteData (dbDataToVoteRecords dbData) rdata
        $(widgetFile "pool")

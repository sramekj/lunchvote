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
        deleteWhere ([] :: [Filter Pool])
        forM rList $ \k -> do
            currentDate <- lift $ liftIO getDateStr
            insert $ Pool (Handler.Cache.id k)  0 currentDate

readDb = runDB $ do
    dbData <- selectList[] [Desc PoolVotes]
    return $ (\(Entity _ d)  -> (poolRestaurantId d, poolVotes d, poolDate d)) <$> dbData

validateDb = do
    today <- lift $ liftIO getDateStr
    invalidRecords <- runDB $ selectList [PoolDate !=. today] []
    result <- case invalidRecords of
                        [] -> readDb
                        _ -> do rdata <- lift $ getData
                                _ <- initDb rdata
                                readDb
    return result
                        
getPoolR :: Handler Html
getPoolR = do
    dbData <- runDB $ selectList[] [Desc PoolVotes]
    validatedData <- case dbData of
                        [] -> do rdata <- lift $ getData
                                 _ <- initDb rdata
                                 readDb
                        _ -> validateDb
    defaultLayout $ do
        rdata <- lift $ getData
        let results = getVoteData (dbDataToVoteRecords validatedData) rdata
        $(widgetFile "pool")

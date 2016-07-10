module Handler.Pool where

import Import
import qualified Data.Map as M
import Data.List (sortBy)
import qualified Data.List as L
import Data.Ord (comparing)
import Handler.Cache
import Data.Time.Clock
import Data.Time.Calendar

type VoteRecords = Map Int Int

getDate :: IO (Integer, Int, Int)
getDate = getCurrentTime >>= return . toGregorian . utctDay

printDate :: (Integer, Int, Int) -> Text
printDate (year, month, day) =  
          pack ((show year) ++ "-" ++ (show month) ++ "-" ++ (show day)) 

getDateStr :: IO (Text)
getDateStr = do
    date <- getDate
    let converted = printDate date
    return converted

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

getPoolR :: Handler Html
getPoolR = do
    dbData <- runDB $ selectList[] [Desc PoolVotes]
    validatedData <- case dbData of
                        [] -> do initDb getData 
                                 readDb
                        otherwise -> readDb
    defaultLayout $ do
        let results = getVoteData (dbDataToVoteRecords validatedData) getData
        $(widgetFile "pool")

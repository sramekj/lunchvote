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

getVoteData :: VoteRecords -> MenuList -> [VoteResult]
getVoteData recordMap menuList = 
                        [VoteResult{name="hovno", votes=5}]
    
--reverse . L.sortBy (comparing snd) 
            

getPoolR :: Handler Html
getPoolR = do
    defaultLayout $ do
        let results = getVoteData getDummyVotes ([]::MenuList)
        $(widgetFile "pool")

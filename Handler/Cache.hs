module Handler.Cache where

import Import
import Data.List.Split (splitOn)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as S
import Network.HTTP.Simple
import Prelude (read)

data DailyMenus = DailyMenus
                  { daily_menus :: [DailyMenu],
                    status :: Text
                  } deriving (Show)

data DailyMenu = DailyMenu
              { daily_menu_id :: Integer,
                start_date :: Text,
                end_date :: Text,
                menu_name :: Text,
                dishes :: [Dish]
              } deriving (Show)

data Dish = Dish
            {  dish_id :: Integer,
               dish_name :: Text,
               price :: Text
            } deriving (Show)

instance FromJSON DailyMenus where
    parseJSON (Object o) = 
         DailyMenus <$> o .: "daily_menus"
                    <*> o .: "status"
    parseJSON _ = mzero

instance FromJSON DailyMenu where
    parseJSON (Object o) = 
         DailyMenu <$> (e >>= (.: "daily_menu_id"))
                   <*> (e >>= (.: "start_date"))
                   <*> (e >>= (.: "end_date"))
                   <*> (e >>= (.: "name"))
                   <*> (e >>= (.: "dishes"))
                   where e = o .: "daily_menu"
    parseJSON _ = mzero

instance FromJSON Dish where
    parseJSON (Object o) = 
         Dish <$> (e >>= (.: "dish_id"))
              <*> (e >>= (.: "name"))
              <*> (e >>= (.: "price"))
              where e = o .: "dish"
    parseJSON _ = mzero

data Meal = Meal 
        { title :: Text,
          mealPrice :: Text
        }

data Menu = Menu
        { id :: Int,
          restaurant :: Text,
          meals :: [Meal]
        }

type MenuList = [Menu] 

data Restaurant = Restaurant Int Text
                    deriving Show

getRestaurantId :: Restaurant -> Int
getRestaurantId (Restaurant id _) = id

getRestaurantTitle :: Restaurant -> Text
getRestaurantTitle (Restaurant _ title) = title

parseRestaurant :: [String] -> Restaurant
parseRestaurant (a:b:[]) = Restaurant (read a :: Int) (pack b)
parseRestaurant _ = error "Cannot parse list of restaurants"

getRestaurants :: IO [Restaurant]
getRestaurants = do
    content <- readFile "./Handler/restaurants.dat" 
    let datalines = lines content
    return $ parseRestaurant . (splitOn "\t") <$>  datalines

apiKey :: ByteString
apiKey = "6a300d56ad8090e3aa3e469048dbdb78"

buildRequest :: String -> String
buildRequest = (++) "GET https://developers.zomato.com/api/v2.1/dailymenu?res_id="

getJSON :: String -> IO (Maybe DailyMenus)
getJSON rId = do
    req' <- parseRequest $ buildRequest rId
    let req = setRequestHeaders [("Accept", "application/json"), ("user_key", apiKey)] $ req'
    response <- httpLBS req
    let respBody = getResponseBody response :: S.ByteString
    let json = decode respBody :: Maybe DailyMenus 
    return json

getData :: IO (MenuList)
getData = do 
    restaurants <- getRestaurants
    forM restaurants processJSON

processJSON :: Restaurant -> IO (Menu)
processJSON r = do
                    json <- getJSON $ show $ getRestaurantId r
                    case json of
                         Nothing -> error "Failed to parse the menu JSON"
                         Just j -> return Menu{id = getRestaurantId r, restaurant = getRestaurantTitle r, meals = getMeals j}                     

getMeals :: DailyMenus -> [Meal]
getMeals menus = let firstMenu = (\(x:_) -> x) $ daily_menus menus
                     meals = dishes firstMenu
                 in (\m -> Meal{title = dish_name m, mealPrice = price m}) <$> meals    


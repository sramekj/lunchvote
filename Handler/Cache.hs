module Handler.Cache where

import Import
import Data.List.Split (splitOn)
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as S
import Network.HTTP.Simple
import Prelude (read)


data DailyMenus = DailyMenus
                  { daily_menus :: [DailyMenu],
                    status :: Text
                  } deriving (Show)

data DailyMenu = DailyMenu
              { daily_menu_id :: Text,
                start_date :: Text,
                end_date :: Text,
                menu_name :: Text,
                dishes :: [Dish]
              } deriving (Show)

data Dish = Dish
            {  dish_id :: Text,
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
          link :: Maybe Text,
          meals :: [Meal]
        }

type MenuList = [Menu] 

data Restaurant = Restaurant Int Text (Maybe Text)
                    deriving Show

getRestaurantId :: Restaurant -> Int
getRestaurantId (Restaurant rid _ _) = rid

getRestaurantTitle :: Restaurant -> Text
getRestaurantTitle (Restaurant _ title _) = title

getRestaurantLink :: Restaurant -> Maybe(Text)
getRestaurantLink (Restaurant _ _ link) = link

parseRestaurant :: [String] -> Restaurant
parseRestaurant (a:b:[]) = Restaurant (read a :: Int) (pack b) Nothing
parseRestaurant (a:b:c:[]) = Restaurant (read a :: Int) (pack b) (Just $ pack c)
parseRestaurant _ = error "Cannot parse list of restaurants"

getRestaurants :: String -> IO [Restaurant]
getRestaurants path = do
    content <- readFile path    
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
    -- putStrLn $ pack ("response: " ++ (S.toString respBody))
    let result = eitherDecode respBody :: Either String DailyMenus 
    case result of
           Left msg -> do  
                          putStrLn $ pack msg
                          return Nothing
           Right jsn -> return $ Just jsn

getData :: IO (MenuList)
getData = do 
    restaurants <- getRestaurants "./Handler/restaurants.dat" 
    adhocs <- getRestaurants "./Handler/adhoc.dat"
    let jsonData = forM restaurants processJSON
    let adhocData = forM adhocs processAdhoc
    jsonData ++ adhocData

processAdhoc :: Restaurant -> IO (Menu)
processAdhoc r = return Menu{id = getRestaurantId r, restaurant = getRestaurantTitle r, link = getRestaurantLink r, meals = []}

processJSON :: Restaurant -> IO (Menu)
processJSON r = do
                    jsonData <- getJSON $ show $ getRestaurantId r
                    case jsonData of
                           Nothing -> return Menu{id = getRestaurantId r, restaurant = getRestaurantTitle r, link = getRestaurantLink r, meals = [] } 
                           Just j -> return Menu{id = getRestaurantId r, restaurant = getRestaurantTitle r, link = getRestaurantLink r, meals = getMeals j }                     

getMeals :: DailyMenus -> [Meal]
getMeals menus = let firstMenu = safeHead $ daily_menus menus
                     meals = case firstMenu of
                                    Just m -> dishes m
                                    Nothing -> []
                 in (\m -> Meal{title = dish_name m, mealPrice = price m}) <$> meals  

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

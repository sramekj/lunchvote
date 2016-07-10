module Handler.Cache where

import Import
import Data.Map()
import qualified Data.Map as M
   
data Meal = Meal 
        { title :: Text,
          price :: Double
        }

data Menu = Menu
        { id :: Int,
          restaurant :: Text,
          meals :: [Meal]
        }

type MenuList = [Menu] 


getData :: MenuList
getData = [Menu{id=0,restaurant="Potrefena husa",meals=[Meal{title="Rizek", price=101}, Meal{title="Kachna", price=128}]},Menu{id=1,restaurant="Coolna",meals=[Meal{title="Parek", price=40}, Meal{title="Sracka", price=150}]},Menu{id=2,restaurant="Harryho restaurant",meals=[Meal{title="Rizoto", price=80}, Meal{title="Smazak", price=99}]}]



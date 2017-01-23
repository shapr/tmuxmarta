{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Lib ( main ) where

import           Control.Arrow        ((&&&))
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as BSL
import           Data.Char            (toLower)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (groupBy, intercalate, nub, sortOn)
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import           GHC.Generics
import           Network.Wreq
import           System.Directory     (getHomeDirectory)

{- config will need to know the line for some people so match on line as well! feature request?
desired output: M:2,8 N:3,7 -}

config = [("MIDTOWN STATION",N), ("NORTH AVE STATION",S), ("LINDBERGH STATION",S)]

url = "http://developer.itsmarta.com/RealtimeTrain/RestServiceNextTrain/GetRealtimeArrivals?apikey="

jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key,val) = (T.toLower key, val)
jsonLower x = x

data Direction = N | E | S | W deriving (Show, Eq, Generic)

instance FromJSON Direction where
  parseJSON = genericParseJSON defaultOptions

data Line = BLUE | GOLD | GREEN | RED deriving (Show, Eq, Generic)

instance FromJSON Line where
  parseJSON = genericParseJSON defaultOptions


-- data Event =
data Event = Event {
  _waiting_time      :: String
  , _waiting_seconds :: String
  , _train_id        :: String
  , _station         :: String
  , _next_arr        :: String
  , _line            :: Line
  , _event_time      :: String
  , _direction       :: Direction
  , _destination     :: String
  } deriving (Show, Generic, Eq)

makeLenses ''Event

instance FromJSON Event where
  parseJSON = genericParseJSON opts . jsonLower
    where opts = defaultOptions { fieldLabelModifier = map toLower . drop 1}

foo = do contents <- BSL.readFile "input.txt"
         print $ (decode contents :: Maybe [Event])

dec s = do contents <- BSL.readFile s
           return (decode contents :: Maybe [Event])

matchStation :: String -> Direction -> Event -> Bool
matchStation s d e = view station e == s && view direction e == d

checks = uncurry matchStation <$> config
my_stations es cs = [(view waiting_time e, view station e) | e <- es , c <- cs , c e]

snip (x,y) = (takeWhile (/= ' ') x,take 1 y)
type Rev = Response [Event]

main = do
  homedir <- getHomeDirectory
  apikey <- readFile (homedir ++ "/.martakey")
  r <- asJSON =<< get (url ++ apikey) :: IO Rev
  print $ map splot $ map (desig &&& mins) (chunkit $ my_stations (r ^. responseBody) checks)

chunkit c = map (take 2) $ Data.List.groupBy (\x y -> snd y == snd x) $ sortOn snd $ snip <$> c

desig as = head . nub $ map snd as
mins as = intercalate "," $ fst <$> as

splot (a,b) = a ++ ":" ++ b

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TransformListComp #-}
module Lib ( main ) where

import           Control.Lens        (Getter, makeLenses, to, (%~), (^.))
import           Data.Aeson.Lens     (_Object)
import           Data.Aeson.Types    (FromJSON (parseJSON),
                                      Options (fieldLabelModifier), Value,
                                      defaultOptions, genericParseJSON)
import           Data.Char           (toLower)
import qualified Data.HashMap.Strict as HM
import           Data.List           (intercalate)
import qualified Data.Text           as T
import           GHC.Exts            (groupWith, the)
import           GHC.Generics        (Generic)
import           Network.Wreq        (asJSON, get, responseBody)
import           System.Directory    (getHomeDirectory)

{- config will need to know the line for some people so match on line as well! feature request?
desired output: M:2,8 N:3,7 -}

config = [("MIDTOWN STATION",N), ("NORTH AVE STATION",S), ("LINDBERGH STATION",S)]

url = "http://developer.itsmarta.com/RealtimeTrain/RestServiceNextTrain/GetRealtimeArrivals?apikey="

data Direction = N | E | S | W deriving (Show, Eq, Generic)

instance FromJSON Direction where
  parseJSON = genericParseJSON defaultOptions

data Line = BLUE | GOLD | GREEN | RED deriving (Show, Eq, Generic)

instance FromJSON Line where
  parseJSON = genericParseJSON defaultOptions

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
waiting_minutes :: Getter Event String
waiting_minutes = waiting_time . to (takeWhile (/=' '))

instance FromJSON Event where
  parseJSON = genericParseJSON opts . jsonLower
    where opts = defaultOptions { fieldLabelModifier = map toLower . drop 1}

jsonLower :: Value -> Value
jsonLower = _Object %~ mapKeys T.toLower
  where mapKeys f hash = HM.fromList [(f k,v) | (k,v) <- HM.toList hash]

main = do
  homedir <- getHomeDirectory
  apikey <- readFile (homedir ++ "/.martakey")
  r <- asJSON =<< get (url ++ apikey)
  print $ [take 1 (the station)++":"++intercalate "," (take 2 minute)
          | event <- r ^. responseBody
          , station <- [event ^. station]
          , (station, event ^. direction) `elem` config
          , let minute = event ^. waiting_minutes
          , then group by station using groupWith]

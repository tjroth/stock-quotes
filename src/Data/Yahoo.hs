{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-} 

module Data.Yahoo where
       
import Network.Wreq
import qualified  Network.HTTP.Client as HC
import Control.Lens
import Data.Text
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Data.Time.Calendar
import Data.Time
import Data.Csv
import qualified Data.Vector as V
import Control.Monad (mzero)
import qualified Control.Exception as E
import Data.Typeable
import Data.Aeson.Lens (_String, key)
import Control.Monad.Except
import Control.Error


data YahooQuote = YahooQuote { yDate :: !Day,
                               yOpen :: !Double,
                               yHigh :: !Double,
                               yLow :: !Double, 
                               yClose :: !Double,
                               yVolume :: !Integer,
                               yAdjClose :: !Double } deriving (Show, Eq)

type Symbol = String

data Interval = Daily | Weekly | Monthly deriving (Eq, Ord)

instance Show Interval where
  show Daily = "d"
  show Weekly = "w"
  show Monthly = "m"

instance FromRecord YahooQuote where
  parseRecord v
    | V.length v == 7 = YahooQuote <$> v .! 0
                        <*> v .! 1
                        <*> v .! 2
                        <*> v .! 3
                        <*> v .! 4
                        <*> v .! 5
                        <*> v .! 6
    | otherwise = mzero

instance FromField Day where
  parseField d = pure $ parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (C8.unpack d)

data YahooApiError = NetworkError HC.HttpException
                   | ParseError String deriving (Show)

type ErrorM = ExceptT YahooApiError IO
               
-- | Request historical quote data from Yahoo server and return as a bytestring (csv formatted).
-- Throws an HttpException if the request fails.
getCSV :: Symbol
          -- ^ The stock ticker symbol
       -> Interval
          -- ^ The quote interval i.e. Daily, Weekly or Monthly
       -> Day
          -- ^ The last day in the range of data to be requested
       -> Integer
          -- ^ How many days of data to request
       -> IO BL.ByteString
getCSV sym intvl endday numdays = do
  r <- getWith opts baseUrl -- :: IO (Either HC.HttpException (Response ByteString))
  return $ r ^. responseBody
  where
      baseUrl = "http://real-chart.finance.yahoo.com/table.csv"     
      stday = addDays (-(numdays-1)) endday
      (f,d,e) = toGregorian endday
      (c,a,b) = toGregorian stday
      opts = defaults &
           param "a" .~ [T.pack . show $ a-1] &
           param "b" .~ [T.pack $ show b] &
           param "c" .~ [T.pack $ show c] &
           param "d" .~ [T.pack . show $ d-1] &
           param "e" .~ [T.pack $ show e] &
           param "f" .~ [T.pack $ show f] &
           param "ignore" .~ [".csv"] &
           param "s" .~ [T.pack sym] &
           param "g" .~ [T.pack $ show intvl]

toQuotes :: BL.ByteString -> Either String (V.Vector YahooQuote)
toQuotes = decode HasHeader

getQuotes :: Symbol
          -- ^ The stock ticker symbol
       -> Interval
          -- ^ The quote interval i.e. Daily, Weekly or Monthly
       -> Day
          -- ^ The last day in the range of data to be requested
       -> Integer
          -- ^ How many days of data to request
       -> IO (Either String (V.Vector YahooQuote)) --(Symbol, Either String (V.Vector YahooQuote))
getQuotes sym intvl endday numdays = do
  bs <- getCSV sym intvl endday numdays
  return $ decode HasHeader bs


{--
getQuotes :: Symbol
          -- ^ The stock ticker symbol
       -> Interval
          -- ^ The quote interval i.e. Daily, Weekly or Monthly
       -> Day
          -- ^ The last day in the range of data to be requested
       -> Integer
          -- ^ How many days of data to request
       -> IO (Either String (V.Vector YahooQuote)) --(Symbol, Either String (V.Vector YahooQuote))
getQuotes sym intvl endday numdays = do
  r <- runExceptT $ do
    bs <- syncIO (getCSV sym intvl endday numdays)
    let qs = decode HasHeader r
    case qs of
     Right r -> return r
     Left e -> return $ Left ("Network Error: " ++ show e)
  return r
--}
  
-- | Request historical quote data from Yahoo server and return as a bytestring (csv formatted).
-- Throws an HttpException if the request fails.
getCSV' :: Symbol
          -- ^ The stock ticker symbol
       -> Interval
          -- ^ The quote interval i.e. Daily, Weekly or Monthly
       -> Day
          -- ^ The last day in the range of data to be requested
       -> Integer
          -- ^ How many days of data to request
       -> ErrorM BL.ByteString --ExceptT YahooApiError IO BL.ByteString
getCSV' sym intvl endday numdays = do
  (r :: Either HC.HttpException (Response BL.ByteString)) <- liftIO (E.try (getWith opts baseUrl)) 
  case r of
   Right r' -> return (r' ^. responseBody)
   Left e -> throwError (NetworkError e)
  where
      baseUrl = "http://real-chart.finance.yahoo.com/table.csv"     
      stday = addDays (-(numdays-1)) endday
      (f,d,e) = toGregorian endday
      (c,a,b) = toGregorian stday
      opts = defaults &
           param "a" .~ [T.pack . show $ a-1] &
           param "b" .~ [T.pack $ show b] &
           param "c" .~ [T.pack $ show c] &
           param "d" .~ [T.pack . show $ d-1] &
           param "e" .~ [T.pack $ show e] &
           param "f" .~ [T.pack $ show f] &
           param "ignore" .~ [".csv"] &
           param "s" .~ [T.pack sym] &
           param "g" .~ [T.pack $ show intvl]


-- | Request historical quote data from the Yahoo server and return a Right (Vector YahooQuote) if the parse
-- is successful, otherwsie returns Left String.  Will throw an "HttpException" if the server request fails.
getQuotes' :: Symbol
          -- ^ The stock ticker symbol
       -> Interval
          -- ^ The quote interval i.e. Daily, Weekly or Monthly
       -> Day
          -- ^ The last day in the range of data to be requested
       -> Integer
          -- ^ How many days of data to request
       -> ErrorM (V.Vector YahooQuote)
getQuotes' sym intvl endday numdays = do
  bs <- getCSV' sym intvl endday numdays
  let qs = decode HasHeader bs 
  case qs of
   Right v -> return v
   Left s -> throwError (ParseError s)


toQuotes' :: BL.ByteString -> ErrorM (V.Vector YahooQuote)
toQuotes' bs = do
  let qs = decode HasHeader bs
  case qs of
   Right v -> return v
   Left s -> throwError (ParseError s)

  
-- | Request historical quote data from the Yahoo server and return a Right (Vector YahooQuote) if the parse
-- is successful, otherwsie returns Left String.  Will throw an "HttpException" if the server request fails.
getQuotes'' :: Symbol
          -- ^ The stock ticker symbol
       -> Interval
          -- ^ The quote interval i.e. Daily, Weekly or Monthly
       -> Day
          -- ^ The last day in the range of data to be requested
       -> Integer
          -- ^ How many days of data to request
       -> ErrorM (V.Vector YahooQuote)
getQuotes'' sym intvl endday numdays = do
  bs <- getCSV' sym intvl endday numdays
  toQuotes' bs

  

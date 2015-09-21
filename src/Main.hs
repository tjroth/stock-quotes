module Main where

import Control.Monad.Trans.Except
import Data.Time
import Data.Yahoo
import Control.Error


main :: IO ()
main = do
  ct <- getCurrentTime
  let cd = utctDay ct
  q <- runExceptT $ do
       bs <- getCSV "FB" Daily cd 10
       toQuotes bs
       
  q' <- runExceptT $ do
       getCSV "FB" Daily cd 10

  s <- runExceptT $ do
       getQuotes "FBsdfa" Daily cd 10
  s' <- runExceptT $ do
       getQuotes "FB" Daily cd 10


  print s
  print s'



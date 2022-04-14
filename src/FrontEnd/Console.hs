{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run,
    Handle (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot
import Control.Monad (forever)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map

newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }

type RepetitionCount = Int

run :: Handle -> IO ()
run h = do
  TIO.putStrLn "Welcome to the echo-bot!"
  forever $ do
    msg <- TIO.getLine
    response <- EchoBot.respond (hBotHandle h) (EchoBot.MessageEvent msg)
    mapM_ (handleResponse h) response

handleResponse :: Handle -> EchoBot.Response T.Text -> IO ()
handleResponse _ (EchoBot.MessageResponse x) = TIO.putStrLn x
handleResponse h (EchoBot.MenuResponse title options) = do
  TIO.putStrLn title
  rawText <- TIO.getLine
  let number = getNumberFromRawText rawText
  let maybeEvent = Map.lookup number $ Map.fromList options
  _ <- maybe (pure []) (EchoBot.respond (hBotHandle h)) maybeEvent
  pure ()

getNumberFromRawText :: T.Text -> RepetitionCount
getNumberFromRawText text = case (readMaybe . T.unpack $ text) of
  Just x -> x
  _      -> 0
 

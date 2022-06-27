{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FrontEnd.Telegram
  ( run
  , Handle
  , textFromMessage
  , messageFromText
  )
where

import qualified Data.Text as T
import qualified EchoBot
import qualified Logger
import Control.Monad (forever)
import Data.Aeson ((.=),(.:),(.:?),(.!=))
import Data.Aeson.QQ
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Text.Read (readMaybe)
import qualified Network.HTTP.Simple as Simple
import Network.HTTP.Client.Internal ( Request(method,  responseTimeout), ResponseTimeout(ResponseTimeoutNone) )
import Data.ByteString.Char8 (ByteString)
import qualified Network.HTTP.Client as LowLevel
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)

type Handle = EchoBot.Handle IO Message
type HandleMap = M.Map UserId Handle

type UpdateId = Int
type MessageId = Int
type UserId = Int
type ChatId = Int
type Message = T.Text
type RepetitionCount = Int

data Update = Update
  { updateId :: UpdateId
  , messageId :: MessageId
  , userId :: UserId
  , chatId :: ChatId
  , message :: Message
  } deriving Show
instance A.FromJSON Update where
  parseJSON = A.withObject "FromJSON Update" $ \o -> Update
    <$> (o .: "update_id")
    <*> (o .: "message" >>= (.: "message_id"))
    <*> (o .: "message" >>= (.: "from") >>= (.: "id"))
    <*> (o .: "message" >>= (.: "chat") >>= (.: "id"))
    <*> (o .: "message" >>= \o1 -> o1 .:? "text" .!= "")

data Updates = Updates [Update]
instance A.FromJSON Updates where
  parseJSON = A.withObject "FromJSON Updates" $ \o -> Updates
    <$> (o .: "result")

botURL :: String
botURL = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/"

tgTimeout ::String 
tgTimeout = "60"

textFromMessage :: Message -> Maybe T.Text
textFromMessage = Just

messageFromText :: T.Text -> Message
messageFromText = id

getUpdates :: Handle -> UpdateId -> IO [Update]
getUpdates h lastUpdateId = do
  rawJSON <- getUpdatesAsJSON lastUpdateId
  let eitherResult = A.eitherDecodeStrict' rawJSON :: Either String Updates
  result <- eitherToUpdate h eitherResult
  return result

eitherToUpdate :: Handle -> Either String Updates -> IO [Update]
eitherToUpdate h eitherResult = case eitherResult of
  Left err -> do
    Logger.logWarning (EchoBot.hLogHandle h) $ (T.pack err)
    pure []
  Right (Updates updates) -> pure updates

getUpdatesAsJSON :: UpdateId -> IO ByteString
getUpdatesAsJSON lastUpdateId = do
  let requestObject = A.object ["offset" .= show lastUpdateId, "timeout" .= tgTimeout] 
  initialRequest <- Simple.parseRequest $ botURL ++ "getUpdates"
  let requestWithBody = Simple.setRequestBodyJSON requestObject initialRequest
  let request = requestWithBody
        { method = "POST"
        , responseTimeout = ResponseTimeoutNone
        }
  res <- Simple.httpBS request
  return $ Simple.getResponseBody res

insertNewUser :: UserId -> Handle -> HandleMap -> HandleMap
insertNewUser = M.insert

proceedUpdate :: Handle -> IORef HandleMap -> Update -> IO UpdateId
proceedUpdate newUserHandle handleMapRef update = do
  handleMap <- readIORef handleMapRef
  modifyIORef' handleMapRef $ 
    if (userId update) `M.notMember` handleMap
      then insertNewUser (userId update) newUserHandle
      else id
  handleMap' <- readIORef handleMapRef
  let handle = handleMap' M.! (userId update)
  response <- EchoBot.respond handle (EchoBot.MessageEvent (message update))
  mapM_ (handleResponse handle update) response
  pure $ updateId update

run :: Handle -> IO ()
run newUserHandle = do
  handleMapRef <- newIORef (M.empty :: HandleMap)
  lastUpdateIdRef <- newIORef (0 :: Int)
  forever $ do
    lastUpdateId <- readIORef lastUpdateIdRef
    Logger.logDebug (EchoBot.hLogHandle newUserHandle) $ (T.pack . show $ lastUpdateId)
    updates <- getUpdates newUserHandle lastUpdateId
    updateIds <- mapM (proceedUpdate newUserHandle handleMapRef) updates
    let newLastUpdateId = if null updateIds
                          then lastUpdateId
                          else 1 + (last updateIds)
    modifyIORef' lastUpdateIdRef (\_ -> newLastUpdateId)

copyMessage :: Update -> IO ()
copyMessage update = do
  let requestObject = A.object
        [ "chat_id" .= chatId update
        , "from_chat_id" .= chatId update
        , "message_id" .= messageId update
        ] 
  initialRequest <- Simple.parseRequest $ botURL ++ "copyMessage"
  let requestWithBody = Simple.setRequestBodyJSON requestObject initialRequest
  let request = requestWithBody
        { method = "POST"
        }
  _ <- Simple.httpBS request
  pure ()

sendMessage :: ChatId -> Message -> IO ()
sendMessage chat msg = do
  let requestObject = A.object
          [ "chat_id" .= chat
          , "text" .= msg
          ]
  initialRequest <- Simple.parseRequest $ botURL ++ "sendMessage"
  let requestWithBody = Simple.setRequestBodyJSON requestObject initialRequest
  let request = requestWithBody
        { method = "POST"
        }
  _ <- Simple.httpBS request
  pure ()

keySet :: A.Value
keySet = [aesonQQ| {"one_time_keyboard":true,"keyboard":[[{"text":"1"},{"text":"2"},{"text":"3"},{"text":"4"},{"text":"5"}]]} |]

getNumber :: Handle -> Update -> Message -> IO T.Text
getNumber h update title = do
  Logger.logDebug (EchoBot.hLogHandle h) $ "Calling getNumber"
  let requestObject = A.object
          [ "chat_id" .= chatId update
          , "text" .= title
          , "reply_markup" .= keySet
          ]
  initialRequest <- Simple.parseRequest $ botURL ++ "sendMessage"
  let requestWithBody = Simple.setRequestBodyJSON requestObject initialRequest
  let request = requestWithBody
        { method = "POST"
        }
  Logger.logDebug (EchoBot.hLogHandle h) $ showBody . LowLevel.requestBody $ request
  _ <- Simple.httpBS request
  updates <- getUpdates h (1 + updateId update)
  Logger.logDebug (EchoBot.hLogHandle h) $ (T.pack . show $ updates)
  let result = if null updates
        then "0"
        else message . last $ updates
  pure result

showBody :: LowLevel.RequestBody -> T.Text
showBody (LowLevel.RequestBodyLBS bs) = decodeUtf8 . toStrict $ bs
showBody (LowLevel.RequestBodyBS bs) = decodeUtf8 bs
showBody _ = "no RequestBody"

handleResponse :: Handle -> Update -> EchoBot.Response Message -> IO ()
handleResponse _ update (EchoBot.MessageResponse x) = if (message update == x) 
  then copyMessage update
  else sendMessage (chatId update) x
handleResponse h update (EchoBot.MenuResponse title options) = do
  rawText <- getNumber h update title
  let number = getNumberFromRawText rawText
  let maybeEvent = M.lookup number $ M.fromList options
  _ <- maybe (pure []) (EchoBot.respond h) maybeEvent
  pure ()

getNumberFromRawText :: T.Text -> RepetitionCount
getNumberFromRawText text = case (readMaybe . T.unpack $ text) of
  Just x -> x
  _      -> 0


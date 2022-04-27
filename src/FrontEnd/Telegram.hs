module FrontEnd.Telegram
  ( run
  , Handle
  , textFromMessage
  , messageFromText
  )
where

import qualified Data.Text as T
import qualified EchoBot
import Control.Monad (forever)
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Text.Read (readMaybe)

type UserId = Int
type ChatId = Int
type Handle = EchoBot.Handle IO Message
type HandleMap = M.Map UserId Handle

data Message 
  = PlainText T.Text
  | Sticker A.Object
  | Picture A.Array

data Update = Update UserId ChatId Message

textFromMessage :: Message -> Maybe T.Text
textFromMessage (PlainText x) = Just x
-- textFromMessage _ = Nothing
-- uncomment previsious string and delete next two strings when begin use Sticker and Picture
textFromMessage (Sticker _) = Nothing
textFromMessage (Picture _) = Nothing

messageFromText :: T.Text -> Message
messageFromText = PlainText

getUpdates :: IO [Update]
getUpdates = undefined

sendMessage :: Handle -> UserId -> ChatId -> Message -> IO ()
sendMessage = undefined

insertNewUser :: UserId -> Handle -> HandleMap -> HandleMap
insertNewUser = M.insert

proceedUpdate :: Handle -> IORef HandleMap -> Update -> IO ()
proceedUpdate newUserHandle handleMapRef (Update userId chatId message) = do
  handleMap <- readIORef handleMapRef
  modifyIORef' handleMapRef $ 
    if userId `M.notMember` handleMap
      then insertNewUser userId newUserHandle
      else id
  handleMap' <- readIORef handleMapRef
  let handle = handleMap' M.! userId
  response <- EchoBot.respond handle (EchoBot.MessageEvent message)
  mapM_ (handleResponse handle userId chatId) response

run :: Handle -> IO ()
run newUserHandle = do
  handleMapRef <- newIORef (M.empty :: HandleMap)
  forever $ do
    updates <- getUpdates
    mapM_ (proceedUpdate newUserHandle handleMapRef) updates

type RepetitionCount = Int

getNumber :: Handle -> UserId -> ChatId -> IO T.Text
getNumber = undefined

handleResponse :: Handle -> UserId -> ChatId -> EchoBot.Response Message -> IO ()
handleResponse h userId chatId (EchoBot.MessageResponse x) = sendMessage h userId chatId x
handleResponse h userId chatId (EchoBot.MenuResponse title options) = do
  sendMessage h userId chatId $ PlainText title
  rawText <- getNumber h userId chatId
  let number = getNumberFromRawText rawText
  let maybeEvent = M.lookup number $ M.fromList options
  _ <- maybe (pure []) (EchoBot.respond h) maybeEvent
  pure ()

getNumberFromRawText :: T.Text -> RepetitionCount
getNumberFromRawText text = case (readMaybe . T.unpack $ text) of
  Just x -> x
  _      -> 0


{-# LANGUAGE OverloadedStrings #-}

module ConfigurationTypes
  ( FrontEndTypeConfig (..)
  , FrontEndType (..)
  , LoggerConfig (..)
  , LogLevel (..)
  , BotConfig (..)
  , Config (..)
  , configFromYaml
  )
where

import Data.Yaml (FromJSON (..), decodeFileEither, withObject, withText, (.:?), (.!=))
import Data.Text (Text)
import qualified System.IO

configFile :: System.IO.FilePath
configFile = "./config.yaml"

defaultFrontEnd :: FrontEndType
defaultFrontEnd = ConsoleFrontEnd

defaultFrontEndConfig :: FrontEndTypeConfig
defaultFrontEndConfig = FrontEndTypeConfig defaultFrontEnd

defaultRepeatCount, maxRepeatCount :: Int
defaultRepeatCount = 3
maxRepeatCount = 5

defaultRepeatReply, defaultHelpReply :: Text
defaultRepeatReply = "Current count of repetitions is {count}. Enter number for new count (1-5)." :: Text
defaultHelpReply = "Echobot - simple echo bot.\n/help to get this help\n/repeat to set the number of repetitions" :: Text

defaultBotConfig :: BotConfig
defaultBotConfig = BotConfig defaultRepeatCount defaultRepeatReply defaultHelpReply

defaultLogHandler :: System.IO.FilePath
defaultLogHandler = "./echo-bot.log" :: System.IO.FilePath

defaultLevel :: LogLevel
defaultLevel = Warning

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig defaultLogHandler defaultLevel 

defaultConfig :: Config
defaultConfig = Config defaultLoggerConfig defaultFrontEndConfig defaultBotConfig

data FrontEndType
  = ConsoleFrontEnd
  | TelegramFrontEnd
  deriving Show

instance FromJSON FrontEndType where
  parseJSON = withText "FromJSON FrontEndType" $ \t ->
    case t of
      "console"   -> pure ConsoleFrontEnd
      "telegram"  -> pure TelegramFrontEnd
      _           -> pure defaultFrontEnd

newtype FrontEndTypeConfig = FrontEndTypeConfig
  { confFrontEndType  :: FrontEndType
  }

instance FromJSON FrontEndTypeConfig where
  parseJSON = withObject "FromJSON FrontEndTypeConfig" $ \o -> FrontEndTypeConfig
    <$> o .:? "frontend" .!= defaultFrontEnd
    
data LoggerConfig = LoggerConfig
  { confPath  :: FilePath
  , confLevel :: LogLevel
  }

instance FromJSON LoggerConfig where
  parseJSON = withObject "FromJSON LoggerConfig" $ \o -> LoggerConfig
    <$> o .:? "path"      .!= defaultLogHandler
    <*> o .:? "logLevel"  .!= defaultLevel

data LogLevel
  = Debug
  | Info
  | Warning
  | Error

instance FromJSON LogLevel where
  parseJSON = withText "FromJSON LogLevel" $ \t ->
    case t of
      "debug"   -> pure Debug
      "info"    -> pure Info
      "warning" -> pure Warning
      "error"   -> pure Error
      _         -> pure defaultLevel

data BotConfig = BotConfig
  { confRepeatDefault :: Int
  , confRepeatReply   :: Text
  , confHelpReply     :: Text
  }

instance FromJSON BotConfig where
  parseJSON = withObject "FromJSON BotConfig" $ \o -> do
    rawCount    <- o .:? "repeatDefault" .!= defaultRepeatCount
    repeatReply <- o .:? "repeatReply"   .!= defaultRepeatReply
    helpReply   <- o .:? "helpReply"     .!= defaultHelpReply
    let readyCount = restrictCount rawCount
    pure (BotConfig readyCount repeatReply helpReply)

restrictCount :: Int -> Int
restrictCount x
  | x>0 && x<maxRepeatCount = x
  | otherwise = defaultRepeatCount

data Config = Config
  { confLogger      :: LoggerConfig
  , confBotFrontEnd :: FrontEndTypeConfig
  , confBot         :: BotConfig
  }

instance FromJSON Config where
  parseJSON = withObject "FromJSON Config" $ \o -> Config
    <$> o .:? "logger"      .!= defaultLoggerConfig
    <*> o .:? "botFrontEnd" .!= defaultFrontEndConfig
    <*> o .:? "bot"         .!= defaultBotConfig

configFromYaml :: IO Config
configFromYaml = do
  errOrConfig <- decodeFileEither configFile
  let config = case errOrConfig of
                (Right x) -> x
                _         -> defaultConfig
  pure config


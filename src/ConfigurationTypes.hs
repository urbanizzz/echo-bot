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

maxRepeatCount :: Int
maxRepeatCount = 5

data FrontEndType
  = ConsoleFrontEnd
  | TelegramFrontEnd
  deriving Show

instance FromJSON FrontEndType where
  parseJSON = withText "FromJSON FrontEndType" $ \t ->
    case t of
      "console"   -> pure ConsoleFrontEnd
      "telegram"  -> pure TelegramFrontEnd
      _           -> error "Config error: frontend" 

newtype FrontEndTypeConfig = FrontEndTypeConfig
  { confFrontEndType  :: FrontEndType
  }

instance FromJSON FrontEndTypeConfig where
  parseJSON = withObject "FromJSON FrontEndTypeConfig" $ \o -> FrontEndTypeConfig
    <$> o .:? "frontend" .!= error "Config error: frontend"
    
data LoggerConfig = LoggerConfig
  { confPath  :: FilePath
  , confLevel :: LogLevel
  }

instance FromJSON LoggerConfig where
  parseJSON = withObject "FromJSON LoggerConfig" $ \o -> LoggerConfig
    <$> o .:? "path"      .!= error "Config error: path"
    <*> o .:? "logLevel"  .!= error "Config error: logLevel"

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
      _         -> error "Config error: logLevel"

data BotConfig = BotConfig
  { confRepeatDefault :: Int
  , confRepeatReply   :: Text
  , confHelpReply     :: Text
  , confBotURL        :: String
  }

instance FromJSON BotConfig where
  parseJSON = withObject "FromJSON BotConfig" $ \o -> do
    rawCount    <- o .:? "repeatDefault" .!= error "Config error: repeatDefault"
    repeatReply <- o .:? "repeatReply"   .!= error "Config error: repeatReply"  
    helpReply   <- o .:? "helpReply"     .!= error "Config error: helpReply"    
    botURL      <- o .:? "botURL"        .!= error "Config error: botURL"       
    let readyCount = restrictCount rawCount
    pure (BotConfig readyCount repeatReply helpReply botURL)

restrictCount :: Int -> Int
restrictCount x
  | x>0 && x<maxRepeatCount = x
  | otherwise = error "Config error: repeatDefault"

data Config = Config
  { confLogger      :: LoggerConfig
  , confBotFrontEnd :: FrontEndTypeConfig
  , confBot         :: BotConfig
  }

instance FromJSON Config where
  parseJSON = withObject "FromJSON Config" $ \o -> Config
    <$> o .:? "logger"      .!= error "Config error: logger"     
    <*> o .:? "botFrontEnd" .!= error "Config error: botFrontEnd"
    <*> o .:? "bot"         .!= error "Config error: bot"        

configFromYaml :: IO Config
configFromYaml = do
  errOrConfig <- decodeFileEither configFile
  let config = case errOrConfig of
                (Right x) -> x
                (Left e)  -> error $ "Config error: " ++ (show e)
  pure config


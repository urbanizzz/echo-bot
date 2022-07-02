{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified ConfigurationTypes
import qualified EchoBot
import qualified Logger.Impl
import Logger (Level (..))
import qualified System.IO

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = do
  (ConfigurationTypes.Config _ _ bot) <- ConfigurationTypes.configFromYaml
  pure EchoBot.Config 
    { EchoBot.confHelpReply = ConfigurationTypes.confHelpReply bot
    , EchoBot.confRepeatReply = ConfigurationTypes.confRepeatReply bot
    , EchoBot.confRepetitionCount = ConfigurationTypes.confRepeatDefault bot
    , EchoBot.confBotURL = ConfigurationTypes.confBotURL bot
    }

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  (ConfigurationTypes.Config logger _ _) <- ConfigurationTypes.configFromYaml
  logHandler <- getLogHandler $ ConfigurationTypes.confPath logger
  let minLevel = getMinLevel $ ConfigurationTypes.confLevel logger
  pure Logger.Impl.Config
    { Logger.Impl.confFileHandle  = logHandler
    , Logger.Impl.confMinLevel    = minLevel
    }

getLogHandler :: System.IO.FilePath -> IO System.IO.Handle
getLogHandler "stderr" = pure System.IO.stderr
getLogHandler "stdout" = pure System.IO.stdout
getLogHandler file = do
  h <- System.IO.openFile file System.IO.AppendMode
  System.IO.hSetBuffering h System.IO.LineBuffering
  pure h

getMinLevel :: ConfigurationTypes.LogLevel -> Level
getMinLevel ConfigurationTypes.Debug    = Debug
getMinLevel ConfigurationTypes.Info     = Info
getMinLevel ConfigurationTypes.Warning  = Warning
getMinLevel ConfigurationTypes.Error    = Error

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = do
  (ConfigurationTypes.Config _ front _) <- ConfigurationTypes.configFromYaml
  pure . ConfigurationTypes.confFrontEndType $ front


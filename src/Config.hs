{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified ConfigurationTypes as CT
import qualified EchoBot
import qualified Logger.Impl
import Logger (Level (..))
import qualified System.IO

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = do
  (CT.Config _ _ bot) <- CT.configFromYaml
  pure EchoBot.Config 
    { EchoBot.confHelpReply = CT.confHelpReply bot
    , EchoBot.confRepeatReply = CT.confRepeatReply bot
    , EchoBot.confRepetitionCount = CT.confRepeatDefault bot
    }

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  (CT.Config logger _ _) <- CT.configFromYaml
  logHandler <- getLogHandler $ CT.confPath logger
  let minLevel = getMinLevel $ CT.confLevel logger
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

getMinLevel :: CT.LogLevel -> Level
getMinLevel CT.Debug    = Debug
getMinLevel CT.Info     = Info
getMinLevel CT.Warning  = Warning
getMinLevel CT.Error    = Error

getFrontEndType :: IO CT.FrontEndType
getFrontEndType = do
  (CT.Config _ front _) <- CT.configFromYaml
  pure . CT.confFrontEndType $ front


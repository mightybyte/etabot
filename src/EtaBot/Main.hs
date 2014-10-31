{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString as B
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Time
import           Network.SimpleIRC
import           System.IO
import           System.Locale

botName :: String
botName = "etabot"

config :: IrcConfig
config = IrcConfig
    { cAddr = "irc.freenode.net"
    , cPort = 6667
    , cNick = botName
    , cUsername = botName
    , cRealname = botName
    , cPass     = Nothing
    , cChannels = ["#snapframework", "#haskell"]
    , cEvents = [notifySnap
                ,Disconnect reconnector
                ]
    , cCTCPVersion = "SimpleIRC v0.3"
    , cCTCPTime    = fmap (formatTime defaultTimeLocale "%c") getZonedTime
    , cPingTimeoutInterval = 350 * 10^6
    }


onDecodeError descr input = Nothing

subst _    _  [       ] = []
subst from to xs@(a:as) =
    if isPrefixOf from xs
        then to ++ subst from to (drop (length from) xs)
        else a : subst from to as
    where isPrefixOf as bs = and $ zipWith (==) as bs

foo = T.concat . T.splitOn "snapshot" 

reconnector mirc = do
    ts <- getCurrentTime
    let timeString = formatTime defaultTimeLocale "%F %X" ts
    hPutStrLn stderr (timeString ++ " reconnecting to server...")
    res <- reconnect mirc
    case res of
      Left e -> do
          hPutStrLn stderr "Error reconnecting, waiting for awhile..."
          threadDelay (5 * 10^6)
      Right _ ->
          hPutStrLn stderr "Successfully reconnected."
          

------------------------------------------------------------------------------
notifySnap :: IrcEvent
notifySnap = Privmsg $ \mirc imsg -> do
    let actualMsg = decodeUtf8With onDecodeError $ mMsg imsg
        msg = T.concat $ T.splitOn "snapshot" $ T.toLower $ decodeUtf8With onDecodeError $ mMsg imsg
        speaker = fromMaybe "Someone" (mNick imsg)
        chan = fromMaybe "<unknown>" (mChan imsg)
        checkNotify str target = when (str `T.isInfixOf` msg) $ do
            let out = B.concat
                    [ speaker 
                    , " is talking about " 
                    , encodeUtf8 str 
                    , " in " 
                    , chan 
                    , ": " 
                    , encodeUtf8 actualMsg
                    ]
            sendMsg mirc target out
            putStrLn $ T.unpack $ decodeUtf8 out
    if chan == "#haskell"
      then do checkNotify "snap" "#snapframework"
              checkNotify "heist" "#snapframework"
              checkNotify "xmlhtml" "#snapframework"
              checkNotify "mightybyte" "mightybyte"
      else return ()

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Connecting to server"
    emirc <- connect config False False
    either (error . show) return emirc
    putStrLn "Connection closed."
    main

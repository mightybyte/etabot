{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString as B
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding
import           Network.SimpleIRC
import           System.IO

botName :: String
botName = "etabot"

watchChannels :: [T.Text]
watchChannels = ["#snapframework", "#haskell", "#nothaskell"]

noticeWords :: [T.Text]
noticeWords = ["snap","heist","xmlhtml","tango"]

noticePrefixes :: [T.Text]
noticePrefixes = ["snaplet-"]

config :: IrcConfig
config =
  let cfg = mkDefaultConfig "irc.freenode.net" botName
  in cfg { cPort = 6667
         , cUsername = botName
         , cRealname = botName
         , cChannels = ["#snapframework", "#haskell"]
         , cEvents = [notifySnap]
         }

onDecodeError descr input = Nothing

subst _    _  [       ] = []
subst from to xs@(a:as) =
    if isPrefixOf from xs
        then to ++ subst from to (drop (length from) xs)
        else a : subst from to as
    where isPrefixOf as bs = and $ zipWith (==) as bs

foo = T.concat . T.splitOn "snapshot" 

------------------------------------------------------------------------------
notifySnap :: IrcEvent
notifySnap = Privmsg $ \mirc imsg -> do
    let actualMsg = decodeUtf8With onDecodeError $ mMsg imsg
        speaker = fromMaybe "Someone" (mNick imsg)
        chan = fromMaybe "<unknown>" (mChan imsg)
        res  = eavesdropResponse (decodeUtf8 speaker) (decodeUtf8 chan)
               actualMsg
    maybe (return ()) (sendMsg mirc "#snapframework" . encodeUtf8) res

dropToWord :: T.Text -> T.Text
dropToWord = T.filter (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'])

dropEndPunctuation :: T.Text -> T.Text
dropEndPunctuation = T.reverse . T.dropWhile (`elem` ".,!") . T.reverse

eavesdropResponse :: T.Text -> T.Text -> T.Text -> Maybe T.Text
eavesdropResponse speaker chan msg
  | chan `elem` watchChannels =
    let rWith x = T.concat [ speaker , " is talking about " , x
                           , " in ", chan, ": ", msg]
    in case msgKeywords msg of
      []  -> Nothing
      [k] -> Just $ rWith k
      ks  -> Just . rWith $
             T.concat [ T.intercalate ", " (init ks), " and ", last ks]
  | otherwise = Nothing

msgKeywords :: T.Text -> [T.Text]
msgKeywords msg =
  [k | k <- noticeWords
     , T.toUpper k `elem` map T.toUpper (map dropToWord $ T.words msg)]
  ++
  [dropEndPunctuation w
  | w <- T.words msg
  , any (\k -> T.toUpper k `T.isPrefixOf` T.toUpper w) noticePrefixes]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Connecting to server"
    emirc <- connect config False False
    either (error . show) return emirc
    putStrLn "Connection closed."
    main

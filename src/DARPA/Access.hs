{-# LANGUAGE OverloadedStrings #-}

module DARPA.Access where

--------------------------------------------------------------------------------

import Network.Pcap -- for reading tcpdump files
import Data.UnixTime -- for reading times
import System.Console.Byline -- for command line interaction
import Control.Monad (void) -- for running byline as IO
import Data.Text (Text) -- dependency of byline
import qualified Data.Text as Text -- dependency of byline
import System.Directory -- for filesystem information and access
import qualified Data.ByteString as B -- for manipulating bytestrings
import Net.Packet -- for reading packets
import Net.PacketParsing
import Net.TCP as TCP
import Net.UDP as UDP
import Data.Array.IArray -- for converting bytestrings for use in Net.Packet
import Data.List (intersect, replicate)

--------------------------------------------------------------------------------
-- DATABASE CONNECTION

-- | asks the user for the directory of the DARPA dataset as provided with this 
-- package
getDatasetReference :: IO (Maybe Text)
getDatasetReference = runByline $ do
    let question = "Please enter the path to the " <> ("DARPA 1999" <> underline) <> " Dataset:\n"
    askUntil question Nothing confirmPath
  where
    -- confirmPath :: MonadIO m => Text -> m (Either Stylized Text)
    confirmPath p = do
        let path = Text.unpack p -- turn text to string
        e <- doesPathExist path
        if e
            then do
                ds <- listDirectory path
                if intersect datasetFiles ds == datasetFiles
                    then return (Right $ Text.pack path)
                    else return (Left "Database not complete.")
            else return (Left "That Path does not exist.")

datasetFiles :: [FilePath]
datasetFiles = ["DARPA 1999 - Week 1 - Monday - inside.tcpdump"]
    -- "DARPA 1999 - Week 1 - Monday - inside.tcpdump",
    -- "DARPA 1999 - Week 1 - Monday - outside.tcpdump",
    -- "DARPA 1999 - Week 1 - Tuesday - inside.tcpdump",
    -- "DARPA 1999 - Week 1 - Tuesday - outside.tcpdump",
    -- "DARPA 1999 - Week 1 - Wednesday - inside.tcpdump",
    -- "DARPA 1999 - Week 1 - Wednesday - outside.tcpdump",
    -- "DARPA 1999 - Week 1 - Thursday - inside.tcpdump",
    -- "DARPA 1999 - Week 1 - Thursday - outside.tcpdump",
    -- "DARPA 1999 - Week 1 - Friday - inside.tcpdump",
    -- "DARPA 1999 - Week 1 - Friday - outside.tcpdump"]

--------------------------------------------------------------------------------
-- DATABASE ACCESS

readPackets :: String -> Int -> (Either (TCP.Packet String) (UDP.Packet String) -> IO ()) -> IO Int
readPackets dumpfilepath packets handler = do
    handle <- openOffline dumpfilepath
    dispatchBS handle packets (\h b -> readAPacket h b handler)

readAPacket :: PktHdr -> B.ByteString -> (Either (TCP.Packet String) (UDP.Packet String) -> IO ()) -> IO ()
readAPacket h b handler = do
    let ws = B.unpack b
    let c = listArray (0, length ws-1) ws :: Chunk
    let i = toInPack c
    let tcpP = doParse i :: Maybe (TCP.Packet String)
    let udpP = doParse i :: Maybe (UDP.Packet String)
    case tcpP of
        (Just tcpPacket) -> handler (Left tcpPacket)
        (Nothing) -> case udpP of
            (Just udpPacket) -> handler (Right udpPacket)
            (Nothing) -> return ()

--------------------------------------------------------------------------------
-- DISPLAY FUNCTIONS

printPacket :: Either (TCP.Packet String) (UDP.Packet String) -> IO ()
printPacket (Left p) = void $ runByline $ do
    sayLn $ stylize $ show p
    sayLn $ stylize (replicate 80 '-')
printPacket (Right p) = void $ runByline $ do
    sayLn $ stylize $ show p
    sayLn $ stylize (replicate 80 '-')

printPacketContent :: Either (TCP.Packet String) (UDP.Packet String) -> IO ()
printPacketContent (Left p) = void $ runByline $ do
    let c = TCP.content p
    sayLn $ stylize c
    sayLn $ stylize (replicate 80 '-')
printPacketContent (Right p) = void $ runByline $ do
    let c = UDP.content p
    sayLn $ stylize c
    sayLn $ stylize (replicate 80 '-')

--------------------------------------------------------------------------------
-- EXTENSIONS

stylize = text . Text.pack

{-# LANGUAGE OverloadedStrings,TypeFamilies,Rank2Types,FlexibleInstances #-}
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Maybe
import Data.Monoid
import System.IO
import Network
import Network.URI
import Numeric

-- A very simple proxy server. This server is not designed to offer a high level of
-- security (so shouldn't be used, for example, as a gateway or anywhere where the
-- public or other untrusted users can have access to it), but rather is designed
-- to assist in web development work when you need to write a custom proxy to
-- see what the effect of certain transformations is.

proxyPort :: PortID
proxyPort = PortNumber 8080

instance MonadReader m => MonadReader (ConduitM i o m) where
  type EnvType (ConduitM i o m) = EnvType m
  ask = lift ask
  local = undefined
data ConnectionConsts = ConnectionConsts { ccHandle :: Handle,
                                           _ccHost :: HostName,
                                           _ccPort :: PortNumber }
class (Functor m, MonadIO m, MonadReader m, EnvType(m) ~ ConnectionConsts) => ConnectionConduit m where
  type OutputType m :: *
  liftConduit :: ConduitM BS.ByteString (OutputType m) (ReaderT ConnectionConsts (ResourceT IO)) a -> m a
instance ConnectionConduit (ConduitM BS.ByteString o (ReaderT ConnectionConsts (ResourceT IO))) where
  type OutputType(ConduitM BS.ByteString o (ReaderT ConnectionConsts (ResourceT IO))) = o
  liftConduit = id
instance (ConnectionConduit m, Error e) => ConnectionConduit (ErrorT e m) where
  type OutputType (ErrorT e m) = OutputType(m)
  liftConduit = lift . liftConduit

getHandle :: ConnectionConduit m => m Handle
getHandle = ccHandle <$> ask

newClient :: Handle -> HostName -> PortNumber -> IO ()
newClient remoteHandle remoteHost pno = do
  runResourceT . flip runReaderT (ConnectionConsts remoteHandle remoteHost pno) $
    CB.sourceIOHandle (return remoteHandle) $$ proxySink
  return ()

validateMethod :: BS.ByteString -> Bool
validateMethod = BSC.all (\c -> (c >= 'A' && c <= 'Z'))

validateURL :: BS.ByteString -> Bool
validateURL = BSC.all (\c -> (c /= '\r' && c /= '\n' && c /= '\0'))

sendWithHeader :: ConnectionConduit m =>
                  BS.ByteString -> [(BS.ByteString, BS.ByteString)] ->
                  BS.ByteString -> m ()
sendWithHeader responseCode headers message = do
  h <- getHandle
  liftIO $ BSC.hPutStr h ("HTTP/1.1 " <> responseCode <> "\r\n\
                 \Content-Length: " <> (BSC.pack . show . BS.length $ message)
                  <> "\r\n" <>
                 BS.concat (map (\(n,v) -> n <> ": " <>
                                v <> "\r\n") headers) <> "\r\n" <>
                 message)

sendInvalid :: ConnectionConduit m => m a
sendInvalid = do
  sendWithHeader "400 Invalid Request"
                 [("Connection", "close")]
                 "Your browser sent an invalid request.\r\n"
  fail "Invalid request"

sendBadGateway :: ConnectionConduit m => m a
sendBadGateway = do
  sendWithHeader "502 Bad Gateway"
                 []
                 "The upstream server sent an invalid HTTP response.\r\n"
  fail "Bad Gateway"

sendForbidden :: ConnectionConduit m => m ()
sendForbidden = do
  sendWithHeader "403 Forbidden" [] "Permission denied.\r\n"
  proxySink

consumeNewLine :: ConnectionConduit m => m () -> m ()
consumeNewLine ifMissing = do
  r <- liftConduit CB.head
  case r of
    Just 13 -> do
      n <- liftConduit CB.head
      when (n /= Just 10) $ ifMissing
    Just 10 -> return ()
    _ -> ifMissing

consumeSpace :: ConnectionConduit m => m () -> m ()
consumeSpace ifMissing = do
  r <- liftConduit CL.peek
  case (BSC.head <$> r) of
    Just ' ' -> liftConduit CB.head >> consumeSpace (return ())
    _ -> ifMissing

proxySink :: ConnectionConduit m => m ()
proxySink = do
  meth <- (BSC.map (Data.Char.toUpper) . LBS.toStrict) <$>
         liftConduit (CB.takeWhile (/= (fromIntegral $ ord ' ')) =$= CB.take 20)
  when (BS.length meth > 19 ||
        (not . validateMethod $ meth)) sendInvalid
  consumeSpace sendInvalid
  url <- LBS.toStrict <$>
         liftConduit (CB.takeWhile (/= (fromIntegral $ ord ' ')) =$= CB.take 1000)
  when (BS.length url > 999 ||
        (not . validateURL $ url)) sendInvalid
  consumeSpace sendInvalid
  ver <- (BSC.map (Data.Char.toUpper) . LBS.toStrict) <$>
         liftConduit (CB.takeWhile (\x -> x /= (fromIntegral $ ord '\r') &&
                              x /= (fromIntegral $ ord '\n')) =$= CB.take 20)
  when (ver /= "HTTP/1.1" && ver /= "HTTP/1.0") sendInvalid
  consumeNewLine sendInvalid

  headers <- readHeaders sendInvalid []
  let contentLength =
        fromMaybe 0 $ 
        maybe (Just 0)
              (liftM fst . BSC.readInt) $ lookup "CONTENT-LENGTH" headers
  content <- LBS.toStrict <$> liftConduit (CB.take contentLength)
  
  if meth == "CONNECT" then doConnect url
    else do
      structuredURL <- maybe sendInvalid return (parseURIReference . BSC.unpack $ url)
      case () of
        () | uriIsAbsolute structuredURL ->
          if (uriScheme structuredURL /= "http:")
            then sendForbidden
            else
              doProxyRequest meth structuredURL headers content
        () | meth == "GET" -> do
          doGet headers structuredURL
          proxySink
        () -> sendForbidden

takeOneResponse :: (ConnectionConduit m, OutputType(m) ~ BS.ByteString) => Bool -> m ()
takeOneResponse isHead = void $ (runErrorT :: ErrorT String m () -> m (Either String ())) $ do
  ver <- (BSC.map (Data.Char.toUpper) . LBS.toStrict) <$>
         liftConduit (CB.takeWhile (\x -> x /= (fromIntegral $ ord ' ') &&
                                          x /= (fromIntegral $ ord '\r') &&
                                          x /= (fromIntegral $ ord '\n')) =$=
                      CB.take 20)
  when (ver /= "HTTP/1.1" && ver /= "HTTP/1.0") sendBadGateway
  consumeSpace sendBadGateway
  code <- LBS.toStrict <$>
           liftConduit (CB.takeWhile (\c ->
                                       c /= (fromIntegral $ ord '\r') &&
                                       c /= (fromIntegral $ ord '\n')) =$= CB.take 50)
  when (BS.length code > 49 ||
        (not . validateURL $ code)) sendBadGateway
  consumeNewLine sendBadGateway
  headers <- readHeaders sendBadGateway []
  liftConduit $ C.yield $ "HTTP/1.1 " <> code <> "\r\n" <>
    BS.concat (map (\(n, v) -> n <> ": " <> v <> "\r\n") headers) <>
    "\r\n"
  let te = lookup "TRANSFER-ENCODING" headers
  let isChunked = te == Just "chunked"
  let clength = lookup "CONTENT-LENGTH" headers
  case () of
    () | isHead -> return ()
    () | isChunked -> do
      takeChunks
      takeChunkTrailer
      return ()
    () | Just clField <- clength,
         Just (clengthBytes, _) <- BSC.readInt clField -> do
      s <- liftConduit (LBS.toStrict <$> CB.take clengthBytes)
      liftConduit (C.yield s)
    () | otherwise -> return ()

takeChunks :: (ConnectionConduit m, OutputType(m) ~ BS.ByteString) => m ()
takeChunks = do
  numStr <- LBS.toStrict <$>
              liftConduit (CB.takeWhile (\x -> x /= (fromIntegral $ ord ' ') &&
                                               x /= (fromIntegral $ ord '\r') &&
                                               x /= (fromIntegral $ ord '\n')
                                        ) =$= CB.take 10)
  let num = fromMaybe 0 (fst <$> (listToMaybe . readHex . BSC.unpack) numStr)
  pd <- liftConduit CL.peek
  if (BSC.head <$> pd) == Just ' '
     then do
       consumeSpace sendBadGateway
       extData <- LBS.toStrict <$>
                    liftConduit (CB.takeWhile
                                 (\x -> x /= (fromIntegral $ ord '\r') &&
                                        x /= (fromIntegral $ ord '\n')) =$=
                                 CB.take 100)
       liftConduit $ C.yield (numStr <> " " <> extData <> "\r\n")
    else
       liftConduit $ C.yield (numStr <> "\r\n")
  consumeNewLine sendBadGateway
  chunkData <- LBS.toStrict <$> liftConduit (CB.take num)
  liftConduit (C.yield chunkData)
  when (num /= 0) (liftConduit (C.yield "\r\n") >>
                   consumeNewLine (return ()) >> takeChunks)

takeChunkTrailer :: (ConnectionConduit m, OutputType m ~ BS.ByteString) => m ()
takeChunkTrailer = do
  r <- liftConduit (CB.take 1)
  case () of
    () | r == "" -> liftConduit (C.yield "\r\n")
    () | r == "\r" -> do
      n <- liftConduit (CB.take 1)
      when (n /= "\n") $ sendBadGateway
      liftConduit (C.yield "\r\n")
    () | r == "\n" -> do
      liftConduit (C.yield "\r\n")
      return ()
    _ -> do
      liftConduit (CB.takeWhile (\c -> c /= 10 && c /= 13))
      consumeNewLine sendBadGateway
      takeChunkTrailer

readHeaders :: ConnectionConduit m => m () -> [(BS.ByteString, BS.ByteString)] -> m [(BS.ByteString, BS.ByteString)]
readHeaders ifWrong l = do
  c <- LBS.toStrict <$> liftConduit (CB.take 1)
  liftConduit (leftover c)
  if (c == "\r" || c == "\n")
    then consumeNewLine ifWrong >> return l
    else do
      name <- (BSC.map (Data.Char.toUpper) . LBS.toStrict) <$>
              liftConduit (CB.takeWhile (/= (fromIntegral $ ord ':')) =$= CB.take 200)
      sp <- liftConduit (CB.take 2)
      when (not (validateURL name) || sp /= ": ") sendInvalid
      val <- (LBS.toStrict) <$>
             liftConduit (CB.takeWhile (\x -> x /= (fromIntegral $ ord '\r') &&
                                              x /= (fromIntegral $ ord '\n')) =$=
                          CB.take 1000)
      consumeNewLine ifWrong
      readHeaders ifWrong ((name, val):l)

guardAccessPermissions :: (ConnectionConduit m) => (BS.ByteString, Int) ->
                          m () -> m ()
guardAccessPermissions (_host, port) f = do
  if port `elem` [80,443,8080] then f else sendForbidden

doConnect :: ConnectionConduit m => BS.ByteString -> m ()
doConnect url =
  let (host:p:_) = BSC.split ':' url
      pNo = read (BSC.unpack p)
  in
    guardAccessPermissions (host, pNo) $ do
      liftIO $ putStrLn ("Connect to " ++ (BSC.unpack host) ++ " port " ++ (show pNo))
      outSock <- liftIO $ connectTo (BSC.unpack host) (PortNumber $ fromIntegral pNo)
      h <- getHandle
      liftIO $ BS.hPut h "HTTP/1.0 200 Connection established\r\n\r\n"
      void $ liftIO $ forkIO $ ignoreIOError $ runResourceT $ 
        CB.sourceIOHandle (return outSock) $$ CB.sinkIOHandle (return h)
      liftConduit $ CB.sinkIOHandle (return outSock)

ignoreIOError :: IO () -> IO ()
ignoreIOError m = m `catch` (const (return ()) :: IOException -> IO ())

doProxyRequest :: ConnectionConduit m => 
                  BS.ByteString -> URI -> [(BS.ByteString, BS.ByteString)] ->
                  BS.ByteString -> m ()
doProxyRequest meth uri headers content =
  let
    Just auth = uriAuthority uri
    host = uriRegName auth
    pNo = if null (uriPort auth) then 80 else read (uriPort auth)
  in
   guardAccessPermissions (BSC.pack host, pNo) $ do
     liftIO $ putStrLn $ "Accessing " ++ (show uri) ++ " with method " ++
                          (show meth)     
     outSock <- liftIO $ connectTo host (PortNumber $ fromIntegral pNo)
     liftIO $
       BS.hPutStr outSock (meth <> " " <> (BSC.pack . uriPath $ uri) <>
                           (BSC.pack . uriQuery $ uri) <> " HTTP/1.1\r\n" <>
                           BS.concat (map (\(n, v) -> n <> ": " <> v <> "\r\n")
                                          headers) <>
                           "\r\n" <> content
                          )
     h <- getHandle
     liftConduit . lift $ CB.sourceIOHandle (return outSock) $$
       ((takeOneResponse (meth=="HEAD") =$ CB.sinkHandle h))
     liftIO $ hClose outSock
     proxySink

doGet :: (ConnectionConduit m) =>
         [(BS.ByteString, BS.ByteString)] -> URI -> m ()
doGet _headers _uri = do
  sendWithHeader "404 Not Found" [] "That URI is invalid on this server.\n"
  proxySink

main :: IO ()
main = do
  ssocket <- listenOn proxyPort
  let doAccept = do
        (remoteHandle, remoteHost, pno) <- accept ssocket
        void $ forkIO $ ignoreIOError (newClient remoteHandle remoteHost pno)
        doAccept
  doAccept

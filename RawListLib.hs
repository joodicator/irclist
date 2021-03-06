module RawListLib(Conf(..), ircList) where

import Data.List
import Data.Char
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Concurrent
import System.IO
import System.Exit
import Network

import IRC

data Conf = Conf{
    cHost         :: HostName,
    cPort         :: PortNumber,
    cNick         :: [String],
    cUser         :: String,
    cReal         :: String,
    cDelaySeconds :: Int }
  deriving Show

ircList :: Conf -> IO ()
ircList conf@Conf{cNick=ns}
  = withSocketsDo $ runReaderT ircList' conf{cNick=ns'}
  where ns' = ns ++ [n ++ show i | i <- [2..], n <- ns]

ircList' :: ReaderT Conf IO ()
ircList' = do
    host <- asks cHost; port <- asks cPort
    h <- lift $ connectTo host (PortNumber port)
    lift $ hSetEncoding h ircEncoding
    lift $ hSetEncoding stdout ircEncoding
    lift $ hSetEncoding stderr ircEncoding
    login h
    delaySeconds <- asks cDelaySeconds
    lift $ threadDelay (1000000 * delaySeconds)
    getList h
    lift $ ehPutStrLn h "QUIT"
    lift $ hClose h

login :: Handle -> ReaderT Conf IO ()
login h = do
    nick:_ <- asks cNick; user <- asks cUser; real <- asks cReal
    lift $ ehPutStrLn h $ unwords ["NICK", nick]
    lift $ ehPutStrLn h $ unwords ["USER", user, "0", "0", ':':real]
    login' h

login' :: Handle -> ReaderT Conf IO ()
login' h = do
    msg <- lift $ parseMessage `fmap` ehGetLine h
    case msg of
        (_,"251",_) -> return ()        -- RPL_LUSERCLIENT
        (_,"432",_) -> lift exitFailure -- ERR_ERRONEUSNICKNAME
        (_,"433",_) -> login'' h        -- ERR_NICKNAMEINUSE
        (_,"436",_) -> login'' h        -- ERR_NICKCOLLISION
        m           -> misc m h >> login' h

login'' :: Handle -> ReaderT Conf IO ()
login'' h = do
    _:nicks@(nick:_) <- asks cNick
    lift $ ehPutStrLn h $ unwords ["NICK", nick]
    withReaderT (\c -> c{cNick=nicks}) (login' h)

getList :: Handle -> ReaderT Conf IO ()
getList h = do
    lift $ ehPutStrLn h "LIST"
    getList' h

getList' :: Handle -> ReaderT Conf IO ()
getList' h = do
    line <- lift $ ehGetLine h
    case parseMessage line of
        (_,"322",_:a) -> do -- RPL_LIST
            lift $ putStrLn $ line
            getList' h
        (_,"323",_) -> do -- RPL_LISTEND
            return ()
        m -> misc m h >> getList' h

misc :: (Maybe String, String, [String]) -> Handle -> ReaderT Conf IO ()
misc (_,"PING",a) h = lift $ ehPutStrLn h $ unwords ["PONG", ':' : unwords a]
misc _            _ = return ()

ehPutStrLn :: Handle -> String -> IO ()
ehPutStrLn h s = do
    hPutStrLn h s
    hPutStrLn stderr $ "\27[33m< " ++ s ++ "\27[0m"

ehGetLine :: Handle -> IO String
ehGetLine h = do
    s <- hGetLine h
    hPutStrLn stderr $ "\27[36m> " ++ s ++ "\27[0m"
    return s

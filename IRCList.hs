module IRCList(Conf(..), ircList) where

import Data.List
import Data.Char
import Control.Monad.Reader
import System.IO
import System.Exit
import Network

data Conf = Conf{
    cHost :: HostName,
    cPort :: PortNumber,
    cNick :: [String],
    cUser :: String,
    cReal :: String }
  deriving Show

ircList :: Conf -> IO ()
ircList conf@Conf{cNick=ns}
  = withSocketsDo $ runReaderT ircList' conf{cNick=ns'}
  where ns' = ns ++ [n ++ show i | i <- [2..], n <- ns]

ircList' :: ReaderT Conf IO ()
ircList' = do
    host <- asks cHost; port <- asks cPort
    h <- lift $ connectTo host (PortNumber port)
    login h
    l <- getList h
    lift $ ehPutStrLn h "QUIT"
    lift $ hClose h

    let l'  = sortBy (\(_,m,_) (_,n,_) -> compare n m) l
    let l'' = map (\(c,n,t) -> [c, show n, t]) l'
    forM_ l'' $ \r -> do
        lift $ putStrLn $ intercalate "\t" r

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

getList :: Handle -> ReaderT Conf IO [(String,Integer,String)]
getList h = do
    lift $ ehPutStrLn h "LIST"
    l <- getList' h
    return [(chan, read count, unwords topic) | chan : count : topic <- l]

getList' :: Handle -> ReaderT Conf IO [[String]]
getList' h = do
    msg <- lift $ parseMessage `fmap` ehGetLine h
    case msg of
        (_,"322",_:a) -> do -- RPL_LIST
            l <- getList' h
            return (a : l)
        (_,"323",_) -> do -- RPL_LISTEND
            return []
        m -> misc m h >> getList' h

misc :: (Maybe String, String, [String]) -> Handle -> ReaderT Conf IO ()
misc (_,"PING",a) h = lift $ ehPutStrLn h $ unwords ["PONG", ':' : unwords a]
misc _            _ = return ()

parseMessage :: String -> (Maybe String, String, [String])
parseMessage (':':l)
  = (Just p, c, a)
  where (p,s)   = break isSpace l
        (_,c,a) = parseMessage (dropWhile isSpace s)
parseMessage l
  = (Nothing, c, a++[drop 1 z])
  where (l',z) = break (== ':') l
        c:a    = words l'

ehPutStrLn :: Handle -> String -> IO ()
ehPutStrLn h s = do
    hPutStrLn h s
    hPutStrLn stderr $ "\27[33m< " ++ s ++ "\27[0m"

ehGetLine :: Handle -> IO String
ehGetLine h = do
    s <- hGetLine h
    hPutStrLn stderr $ "\27[36m> " ++ s ++ "\27[0m"
    return s

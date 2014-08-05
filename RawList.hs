import Data.Either
import Data.Maybe
import Data.Char
import System.Environment
import System.Exit
import System.IO
import Network

import RawListLib

main = getConf >>= ircList

defaultPort = 6667
defaultNick = "irclist"
defaultUser = "irclist"
defaultReal = "irclist"

getConf :: IO Conf
getConf = getArgs >>= getConf'

getConf' :: [String] -> IO Conf
getConf' args = do
    (host, uArgs) <- case uArgs of
        host : uArgs' -> return (host, uArgs')
        []            -> exitUsage
    port <- case uArgs of
        []   -> return defaultPort
        [ps] -> case [p | (p,"") <- reads ps] of
            p:_ -> return $ fromInteger p
            []  -> exitString $ "Error: invalid port " ++ show ps ++ "."
        _    -> exitUsage
    delaySeconds <- case lookup "delay" nArgs of
        Just ss -> case [s | (s,"") <- reads ss] of
            s:_ | s >= 0 -> return s
            []           -> exitString $ "Error: invalid delay " ++ show ss ++ "."
        Nothing -> return 0
    let conf = Conf{
        cHost = host,
        cPort = port,
        cNick = [fromMaybe defaultNick $ lookup "nick" nArgs],
        cUser = fromMaybe defaultUser $ lookup "user" nArgs,
        cReal = fromMaybe defaultReal $ lookup "real" nArgs,
        cDelaySeconds = delaySeconds }
    checkConf conf
    return conf
  where
    (uArgs, nArgs)   = partitionEithers (map eArg args)
    eArg ('-':'-':a) = Right (n, drop 1 v) where (n,v) = break (== '=') a
    eArg          a  = Left a

checkConf :: Conf -> IO ()
checkConf conf
    | any illegal (cHost conf)
    = exitString $ "Error: illegal hostname " ++ show (cHost conf) ++ "."
    | any illegal (cUser conf)
    = exitString $ "Error: invalid username " ++ show (cUser conf) ++ "."
    | any isControl (cReal conf)
    = exitString $ "Error: invalid realname " ++ show (cReal conf) ++ "."
    | otherwise
    = case filter (any illegal) (cNick conf) of
        n:_ -> exitString $ "Error: invalid nick " ++ show n ++ "."
        []  -> return ()
  where
    illegal c = isControl c || isSpace c

exitUsage :: IO a
exitUsage = do
    progName <- getProgName
    exitString $ "Usage: " ++ progName ++ " HOST [PORT]" ++
                 " [--nick=NICK] [--user=USER] [--real=REAL]"

exitString :: String -> IO a
exitString s = hPutStrLn stderr s >> exitFailure

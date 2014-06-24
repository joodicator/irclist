import Data.List
import Control.Monad
import System.IO

import Format
import IRC

main = do
    hSetEncoding stdin ircEncoding
    hSetEncoding stdout ircEncoding
    contents <- getContents
    let ls = sortBy (\(m,_,_) (n,_,_) -> compare n m) $ do
        l <- filter (/= '\r') `fmap` lines contents
        (chan,count',topic) <- return $ case parseMessage l of
            (_,_,[_,c,n,t]) -> (c,n,t)
            _             -> error ("cannot parse: " ++ l)
        count <- return $ case [c | (c,"") <- reads count'] of
            []  -> error ("cannot parse " ++ show count')
            c:_ -> c
        return (count :: Integer, chan, formatANSI . unFormatIRC $ topic)
    forM_ ls $ \(n,c,t) -> do
        putStrLn $ intercalate "\t" [show n, c, t]

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p xs
  = ys : split p (drop 1 zs)
  where (ys,zs) = break p xs

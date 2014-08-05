module IRC where

import System.IO
import Data.Char
import Data.List

ircEncoding = char8

parseMessage :: String -> (Maybe String, String, [String])

parseMessage (':':l)
  = (Just p, c, a)
  where 
    (p,s)   = break isSpace l
    (_,c,a) = parseMessage (dropWhile isSpace s)

parseMessage l
  = (Nothing, c, a++[drop 1 z])
  where
    (l',z) = break' l
    c:a    = words' l'
    break' (' ':':':s) = (" ",':':s)
    break' (c:s)       = (c:h,t) where (h,t) = break' s
    break' ""          = ("","")

words' :: String -> [String]
words' (' ':s) = words' (dropWhile (==' ') s)
words' ""      = []
words' s       = let (w,s') = break (==' ') s in w : words' s'

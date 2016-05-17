{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    MultiParamTypeClasses,
    FlexibleInstances,
    FunctionalDependencies #-}

--------------------------------------------------------------------------------
module Format(
    Colour(..), FString, fLength, fGenericLength,
    plain, invert, bold, underline, colour, colourB, colourFB,
    formatPlain, formatANSI, formatIRC, unFormatIRC) where

import Prelude hiding (foldr)
import Data.List hiding (foldr)
import Data.Char hiding (Format)
import Data.Tuple
import Data.Maybe
import Data.Monoid
import Data.Foldable(Foldable(..))
import Data.Function

--------------------------------------------------------------------------------
data Colour
  = LRed | LYellow | LGreen | LCyan | LBlue | LMagenta | LWhite | LBlack
  | DRed | DYellow | DGreen | DCyan | DBlue | DMagenta | DWhite | DBlack
  deriving (Eq, Show)

data Style
  = Invert | Bold | Underline | FColour Colour | BColour Colour
  deriving (Eq, Show)

newtype Format a
  = Format [FormatD a]
  deriving (Monoid, Show)

data FormatD a
  = Plain !a | Style !Style !(Format a)
  deriving Show

type FString = Format String

--------------------------------------------------------------------------------
formatPlain :: Monoid a => Format a -> a
formatPlain = fold

formatANSI :: FString -> String
formatANSI = formatByDiff diff where
    diff :: [Style] -> [Style] -> String
    diff [] [] = ""
    diff ss ss' = case diff' ss ss' of
        "" -> ""
        cs -> "\27[" ++ cs ++ "m"

    diff' :: [Style] -> [Style] -> String
    diff' ss ss'
      = minimumBy (compare `on` length) $ map (intercalate ";") $
        ["0" : start [] ss', stop ss ss' ++ start ss ss']

    stop :: [Style] -> [Style] -> [String]
    stop ss ss' = concatMap (take 1) [
        ["21" | Bold      <- ss, not $ elem Bold      ss'],
        ["24" | Underline <- ss, not $ elem Underline ss'],
        ["27" | Invert    <- ss, not $ elem Invert    ss'],
        ["39" | FColour _ <- ss, null [() | FColour _ <- ss']],
        ["49" | BColour _ <- ss, null [() | BColour _ <- ss']]]

    start :: [Style] -> [Style] -> [String]
    start ss ss' = concatMap (take 1) [
        ["1" | Bold      <- ss', not $ elem Bold      ss],
        ["4" | Underline <- ss', not $ elem Underline ss],
        ["7" | Invert    <- ss', not $ elem Invert    ss],
        [show $ fc c | f@(FColour c) <- reverse ss',
                       [f] /= [f' | f'@(FColour _) <- reverse ss]],
        [show $ bc c | f@(BColour c) <- reverse ss',
                       [f] /= [f' | f'@(BColour _) <- reverse ss]]]

    fc :: Colour -> Int
    fc = fromJust . flip lookup ansiColours

    bc :: Colour -> Int
    bc = (+10) . fc

formatIRC :: FString -> String
formatIRC  = formatByDiff diff where
    diff = undefined

formatByDiff :: Monoid a => ([Style] -> [Style] -> a) -> Format a -> a
formatByDiff d f
  = let (ss,x) = sfoldr g ([],mempty) f in []`d`ss <> x
  where g (ss,x) (ss',x') = (ss, x <> ss`d`ss' <> x')

--------------------------------------------------------------------------------
unFormatIRC :: String -> FString
unFormatIRC = Format . parse [] where
    ccode :: String -> (Maybe Colour, Maybe Colour, String)
    ccode s = case ccode' s of
        (mf,',':s') -> (mf, mb, s'') where (mb,s'') = ccode' s'
        (mf,    s') -> (mf, Nothing, s')

    ccode' :: String -> (Maybe Colour, String)
    ccode' (c:d:s) | all isDigit [c,d] = (ccode'' (read [c,d]), s)
    ccode'   (c:s) | isDigit c         = (ccode'' (read [c]),   s)
    ccode'      s                      = (Nothing,              s)

    ccode'' :: Int -> Maybe Colour
    ccode'' = flip lookup (map swap ircColours)

    parse :: [Style] -> String -> [FormatD String]
    parse ss ('\03':s)
      = parse (ss'' ++ ss') s'
      where
        (mf,mb,s') = ccode s
        ss'  = ss \\ ([t | t@(FColour _) <- ss] ++ [t | t@(BColour _) <- ss])
        ss'' = fmap FColour (maybeToList mf) ++ fmap BColour (maybeToList mb)
    parse ss ('\02':s') -- Bold
      | Bold `elem` ss      = parse (ss \\ [Bold]) s'
      | otherwise           = parse (Bold : ss) s'
    parse ss ('\31':s') -- Underline
      | Underline `elem` ss = parse (ss \\ [Underline]) s'
      | otherwise           = parse (Underline : ss) s'
    parse ss ('\22':s') -- Invert
      | Invert `elem` ss    = parse (ss \\ [Invert]) s'
      | otherwise           = parse (Invert : ss) s'
    parse ss ('\15':s') -- Reset
      = parse [] s'
    parse _ []
      = []
    parse ss s
      = foldr (\t d -> Style t (Format [d])) (Plain h) ss : parse ss t
      where
        (h,t) = break (`elem` "\03\02\31\22\15") s

--------------------------------------------------------------------------------
ircColours :: [(Colour,Int)]
ircColours = [
    (DBlack, 01), (DRed,     05), (DGreen, 03), (DYellow, 07),
    (DBlue,  02), (DMagenta, 06), (DCyan,  10), (DWhite,  15),
    (LBlack, 14), (LRed,     04), (LGreen, 09), (LYellow, 08),
    (LBlue,  12), (LMagenta, 13), (LCyan,  11), (LWhite,  00)]

ansiColours :: [(Colour,Int)]
ansiColours = [
    (DBlack, 30), (DRed,     31), (DGreen, 32), (DYellow, 33),
    (DBlue,  34), (DMagenta, 35), (DCyan,  36), (DWhite,  37),
    (LBlack, 90), (LRed,     91), (LGreen, 92), (LYellow, 93),
    (LBlue,  94), (LMagenta, 95), (LCyan,  96), (LWhite,  97)]

--------------------------------------------------------------------------------
sfoldr :: (([Style],a) -> b -> b) -> b -> Format a -> b
sfoldr g = ssfoldr [] where
    ssfoldr ss z (Format ds) = foldr (sdfoldr ss) z ds
    sdfoldr ss (Plain x)   z = g (ss,x) z
    sdfoldr ss (Style s f) z = ssfoldr (s:ss) z f

instance Foldable Format where
    foldr = sfoldr . (. snd)

fLength :: Format [a] -> Int
fLength = foldr ((+) . length) 0

fGenericLength :: Num n => Format [a] -> n
fGenericLength = foldr ((+) . genericLength) 0

--------------------------------------------------------------------------------
class FormatC a b | a -> b where
    plain  :: a -> Format b
    format :: Style -> a -> Format b

instance FormatC (Format a) a where
    plain      = id
    format s f = Format [Style s f]

instance FormatC String String where
    plain x  = Format [Plain x]
    format s = format s . plain

--------------------------------------------------------------------------------
invert :: FormatC a b => a -> Format b
invert = format Invert

bold :: FormatC a b => a -> Format b
bold = format Bold

underline :: FormatC a b => a -> Format b
underline = format Underline

colour :: FormatC a b => Colour -> a -> Format b
colour = format . FColour

colourB :: FormatC a b => Colour -> a -> Format b
colourB = format . BColour

colourFB :: FormatC a b => (Colour, Colour) -> a -> Format b
colourFB (f,b) = colour f . colourB b

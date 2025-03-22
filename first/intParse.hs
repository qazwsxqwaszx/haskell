-- file: ch04/IntParse.hs 
import Data.Char (digitToInt) -- we'll need ord shortly

asInt :: String -> Int
loop :: Int -> String -> Int
loop acc [] = acc
asInt xs = loop 0 xs
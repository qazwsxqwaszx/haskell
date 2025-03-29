module Markdown
  ( parseMarkdown
  ) where

import Markup
import Numeric.Natural (Natural)

parseMarkdown :: String -> Document
parseMarkdown = parseLines . lines

parseLines :: [String] -> Document
parseLines [] = []
parseLines (line:rest)
  | isHeading line      = let (lvl, txt) = parseHeading line
                           in Heading lvl txt : parseLines rest
  | isListItem line     = let (items, rest') = span isListItem (line:rest)
                           in UnorderedList (map parseListItem items) : parseLines rest'
  | isBlank line        = parseLines rest
  | otherwise           = let (paraLines, rest') = break isBlank (line:rest)
                           in Paragraph (unwords (line : paraLines)) : parseLines rest'

-- 判斷類型
isHeading :: String -> Bool
isHeading ('#':_) = True
isHeading _       = False

isListItem :: String -> Bool
isListItem ('-':' ':_) = True
isListItem _           = False

isBlank :: String -> Bool
isBlank = all (`elem` " \t")

-- 解析實際內容
parseHeading :: String -> (Natural, String)
parseHeading str = let (hashes, rest) = span (== '#') str
                   in (fromIntegral $ length hashes, dropWhile (== ' ') rest)

parseListItem :: String -> String
parseListItem = drop 2

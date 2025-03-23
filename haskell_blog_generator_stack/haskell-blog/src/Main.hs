{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
  let url = "https://802.mnd.gov.tw/"
  response <- httpLBS url
  putStrLn $ "Status code: " <> show (getResponseStatusCode response)
  putStrLn "Response body:"
  L8.putStrLn (getResponseBody response)



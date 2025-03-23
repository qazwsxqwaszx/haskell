{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import Network.HTTP.Client (Request, requestHeaders)
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
  initRequest <- parseRequest "https://802.mnd.gov.tw/"
  let request = initRequest
        { requestHeaders = [("User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")]
        }
  response <- httpLBS request
  putStrLn $ "Status code: " <> show (getResponseStatusCode response)
  putStrLn "Response body:"
  L8.putStrLn (getResponseBody response)



{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple

main :: IO ()
main = do
  response <- httpLBS "http://example.com"
  putStrLn $ "Status code: " <> show (getResponseStatusCode response)

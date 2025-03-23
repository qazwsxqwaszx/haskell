{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest "http://example.com"
  response <- httpLbs request manager
  putStrLn $ "Status code: " <> show (statusCode $ responseStatus response)


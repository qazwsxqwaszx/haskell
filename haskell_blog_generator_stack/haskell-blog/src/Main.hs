-- {-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE OverloadedStrings #-}

-- import Network.HTTP.Simple
-- import qualified Data.ByteString.Lazy.Char8 as L8

-- main :: IO ()
-- main = do
--   let url = "https://802.mnd.gov.tw/"
--   response <- httpLBS url
--   putStrLn $ "Status code: " <> show (getResponseStatusCode response)
--   putStrLn "Response body:"
--   L8.putStrLn (getResponseBody response)

-- import Control.Monad.State

-- -- 狀態型別是 Int，函數回傳 ()（只做修改）
-- increment :: State Int ()
-- increment = do
--   n <- get       -- 取得目前狀態
--   put (n + 1)    -- 設定新狀態

-- -- 一次加 3
-- addThree :: State Int ()
-- addThree = do
--   increment
--   increment
--   increment

-- main :: IO ()
-- main = do
--   let finalState = execState addThree 0
--   putStrLn $ "最後狀態為: " ++ show finalState
-- import Control.Concurrent.Async
-- main :: IO ()
-- main = do
--   result <- concurrently (return "Hello") (return "World")
--   print result

-- import Options.Applicative

-- main :: IO ()
-- main = do
--   name <- execParser opts
--   putStrLn $ "Hello, " ++ name
--   where
--     opts = info (strArgument (metavar "NAME") <**> helper)
--                 ( fullDesc
--                <> progDesc "Print a greeting for NAME" )
-- import Test.Tasty
-- import Test.Tasty.HUnit

-- newtype Html = Html String
-- newtype Structure = Structure String


-- main :: IO ()
-- main = defaultMain $ testCase "HTML test" (Html "hello" `seq` True @?= True)
-- main :: IO ()
-- main = do
--   -- 測試用
--   defaultMain $ testCase "HTML test" (Html "hello" `seq` True @?= True)

--   -- 額外輸出 HTML 結果
--   let doc = Html "Hello from stack run!"
--   putStrLn (renderHtml doc)

import Test.Tasty
import Test.Tasty.HUnit

-- 型別定義
newtype Html = Html String
newtype Structure = Structure String

-- 組合 HTML 用的函式
p_ :: String -> Structure
p_ content = Structure ("<p>" ++ content ++ "</p>")

html_ :: Structure -> Html
html_ (Structure s) = Html s

-- render Html
renderHtml :: Html -> String
renderHtml (Html s) = "<html>" ++ s ++ "</html>"

-- main 程式
main :: IO ()
main = do
  let body = p_ "Hello from stack run!"
      doc = html_ body
  putStrLn (renderHtml doc)   -- 印出完整 HTML
  defaultMain $ testCase "HTML test" (doc `seq` True @?= True)


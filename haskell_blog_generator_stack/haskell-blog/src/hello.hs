-- main = putStrLn "<html><body>Hello, world!</body></html>"
-- wrapHtml :: String -> String
-- wrapHtml content = "<html><body>" <> content <> "</body></html>"
-- myhtml :: String
-- myhtml = wrapHtml "Hello, world!"
-- main :: IO ()
-- main = putStrLn (wrapHtml "Hello, world!")
-- main = putStrLn wrapHtml "Hello, world!"
--main = putStrLn $ wrapHtml "Hello, world!"
-- 將 wrapHtml 拆分成兩個函數
-- html_ :: String -> String
-- html_ content = "<html>" <> content <> "</html>"

-- body_ :: String -> String
-- body_ content = "<body>" <> content <> "</body>"

-- -- 使用這些函數來定義 myhtml
-- myhtml :: String
-- myhtml = html_ (body_ "Hello, world!")

-- -- 添加 head_ 和 title_ 函數
-- head_ :: String -> String
-- head_ content = "<head>" <> content <> "</head>"

-- title_ :: String -> String
-- title_ content = "<title>" <> content <> "</title>"

-- -- 測試 main，輸出完整的 HTML 結構
-- main :: IO ()
-- main = putStrLn (myhtml)
-- html_ :: String -> String
-- html_ content = "<html>" <> content <> "</html>"

-- body_ :: String -> String
-- body_ content = "<body>" <> content <> "</body>"

-- head_ :: String -> String
-- head_ content = "<head>" <> content <> "</head>"

-- title_ :: String -> String
-- title_ content = "<title>" <> content <> "</title>"

-- -- 定義 makeHtml，接受標題與內容，生成完整的 HTML 結構
-- makeHtml :: String -> String -> String
-- makeHtml titleContent bodyContent = 
--     html_ (head_ (title_ titleContent) <> body_ bodyContent)

-- -- 使用 makeHtml 來更新 myhtml
-- myhtml :: String
-- myhtml = makeHtml "My page title" "My page content"

-- -- 測試 main，輸出完整的 HTML 結構
-- main :: IO ()
-- main = putStrLn myhtml
doNothing :: ()
doNothing = ()
-- main :: IO ()
-- main = putStrLn "Hello, world!"
-- main :: IO ()
-- main = do
--   putStrLn "What's your name?"
--   name <- getLine
--   putStrLn ("Hello, " <> name)

-- main :: IO ()
-- main =
--   putStrLn "What's your name?" >>
--   getLine >>= \name ->
--   putStrLn ("Hello, " <> name)
-- main :: IO ()
-- main = getLine >>= putStrLn . ("Hello, " <>)
-- writeFile :: FilePath -> String -> IO ()
-- main :: IO ()
-- main = do   
--     writeFile "log.txt" "This is a log entry"
-- main :: IO ()
-- main = do
--   let linesToWrite = ["[INFO] Start", "[INFO] Writing log", "[INFO] Done"]
--   writeFile "log.txt" (unlines linesToWrite)
--   putStrLn "已寫入 log.txt，現在讀出內容："
--   content <- readFile "log.txt"
--   putStrLn "內容如下："
--   putStrLn content
makeHtml :: String -> String -> String
makeHtml title body =
  "<html><head><title>" <> title <> "</title></head><body>" <> body <> "</body></html>"




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
html_ :: String -> String
html_ content = "<html>" <> content <> "</html>"

body_ :: String -> String
body_ content = "<body>" <> content <> "</body>"

head_ :: String -> String
head_ content = "<head>" <> content <> "</head>"

title_ :: String -> String
title_ content = "<title>" <> content <> "</title>"

-- 定義 makeHtml，接受標題與內容，生成完整的 HTML 結構
makeHtml :: String -> String -> String
makeHtml titleContent bodyContent = 
    html_ (head_ (title_ titleContent) <> body_ bodyContent)

-- 使用 makeHtml 來更新 myhtml
myhtml :: String
myhtml = makeHtml "My page title" "My page content"

-- 測試 main，輸出完整的 HTML 結構
main :: IO ()
main = putStrLn myhtml

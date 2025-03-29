module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  ) where

-- 型別與結構
newtype Html = Html String
newtype Structure = Structure String
type Title = String

-- 將 Html 或 Structure 解開為 String
render :: Html -> String
render (Html str) = str

getStructureString :: Structure -> String
getStructureString (Structure str) = str

-- 建立 HTML 標籤（不對外公開）
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- 特定標籤產生器
p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

-- 合併兩段 HTML Structure
append_ :: Structure -> Structure -> Structure
append_ c1 c2 = Structure (getStructureString c1 <> getStructureString c2)

-- 組合標題與內容為完整 HTML
html_ :: Title -> Structure -> Html
html_ title content =
  Html
    (el "html"
      (el "head" (el "title" title) <>
       el "body" (getStructureString content)))

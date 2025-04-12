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

-- import Test.Tasty
-- import Test.Tasty.HUnit

-- -- 型別定義
-- newtype Html = Html String
-- newtype Structure = Structure String

-- -- 組合 HTML 用的函式
-- p_ :: String -> Structure
-- p_ content = Structure ("<p>" ++ content ++ "</p>")

-- html_ :: Structure -> Html
-- html_ (Structure s) = Html s

-- -- render Html
-- renderHtml :: Html -> String
-- renderHtml (Html s) = "<html>" ++ s ++ "</html>"

-- -- main 程式
-- main :: IO ()
-- main = do
--   let body = p_ "Hello from stack run!"
--       doc = html_ body
--   putStrLn (renderHtml doc)   -- 印出完整 HTML
--   defaultMain $ testCase "HTML test" (doc `seq` True @?= True)
-- import FHIR.Patient
-- import Data.Time (fromGregorian)
-- import qualified Data.ByteString.Lazy.Char8 as B

-- samplePatient :: Patient
-- samplePatient = Patient
--   { resourceType     = "Patient"
--   , patientId        = Just "example"
--   , patientActive    = Just True
--   , patientName      = [HumanName (Just "official") (Just "Doe") ["John"]]
--   , patientTelecom   = [ContactPoint (Just "phone") (Just "123-4567") (Just "mobile")]
--   , patientGender    = Just Male
--   , patientBirthDate = Just (fromGregorian 1980 1 1)
--   , patientDeceased  = Nothing
--   , patientAddress   = [Address (Just "home") ["123 Main St"] (Just "Somewhere") (Just "CA") (Just "90210") (Just "USA")]
--   , patientMarital   = Just "M"
--   , patientContact   = [Contact (Just (HumanName Nothing (Just "Smith") ["Jane"])) [] (Just Female)]
--   }


-- main :: IO ()
-- main = B.putStrLn (encode samplePatient)
-- module Main where

-- import FHIR.Patient
-- import Data.Time (fromGregorian)
-- import qualified Data.ByteString.Lazy.Char8 as B
-- import Data.Aeson (encode)
-- import qualified Data.Text as T
-- samplePatient :: Patient
-- samplePatient = Patient
--   { patientResourceType  = "Patient"
--   , patientId            = Just "example"
--   , patientActive        = Just True
--   , patientName          = [HumanName (Just "official") (Just "Doe") ["John"]]
--   , patientTelecom       = [ContactPoint (Just "phone") (Just "123-4567") (Just "mobile")]
--   , patientGender        = Just Male
--   , patientBirthDate     = Just (fromGregorian 1980 1 1)
--   , patientDeceased      = Nothing
--   , patientAddress       = [Address (Just "home") ["123 Main St"] (Just "Somewhere") (Just "CA") (Just "90210") (Just "USA")]
--   , patientMaritalStatus = Just "M"
--   , patientContact       = [Contact (Just (HumanName Nothing (Just "Smith") ["Jane"])) [] (Just Female)]
--   }

-- main :: IO ()
-- main = B.putStrLn (encode samplePatient)

-- module Main where

-- import FHIR.Patient
-- import Data.Time (fromGregorian)
-- import qualified Data.ByteString.Lazy.Char8 as B
-- import Data.Aeson (encode)
-- import qualified Data.Text as T  -- ⭐ 加這行！
-- import Data.Char (chr, ord)
-- incrementChar c = chr (ord (id c) + id 1)

-- samplePatient :: Patient
-- samplePatient = Patient
--   { patientResourceType  = T.pack "Patient"
--   , patientId            = Just (T.pack "example")
--   , patientActive        = Just True
--   , patientName          = [HumanName (Just (T.pack "official")) (Just (T.pack "Doe")) [T.pack "John"]]
--   , patientTelecom       = [ContactPoint (Just (T.pack "phone")) (Just (T.pack "123-4567")) (Just (T.pack "mobile"))]
--   , patientGender        = Just Male
--   , patientBirthDate     = Just (fromGregorian 1980 1 1)
--   , patientDeceased      = Nothing
--   , patientAddress       = [Address (Just (T.pack "home")) [T.pack "123 Main St"] (Just (T.pack "Somewhere")) (Just (T.pack "CA")) (Just (T.pack "90210")) (Just (T.pack "USA"))]
--   , patientMaritalStatus = Just (T.pack "M")
--   , patientContact       = [Contact (Just (HumanName Nothing (Just (T.pack "Smith")) [T.pack "Jane"])) [] (Just Female)]
--   }

-- main :: IO ()
-- main = B.putStrLn (encode samplePatient)

-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- -- 型別與結構
-- newtype Html = Html String
-- newtype Structure = Structure String
-- type Title = String

-- -- 將 Html 或 Structure 解開為 String
-- render :: Html -> String
-- render (Html str) = str

-- getStructureString :: Structure -> String
-- getStructureString (Structure str) = str

-- -- 建立 HTML 標籤
-- el :: String -> String -> String
-- el tag content =
--   "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- -- 特定標籤產生器
-- p_ :: String -> Structure
-- p_ = Structure . el "p"

-- h1_ :: String -> Structure
-- h1_ = Structure . el "h1"

-- -- 合併兩段 HTML Structure
-- append_ :: Structure -> Structure -> Structure
-- append_ (Structure a) (Structure b) = Structure (a <> b)

-- -- 組合標題與內容為完整 HTML
-- html_ :: Title -> Structure -> Html
-- html_ title content =
--   Html
--     (el "html"
--       (el "head" (el "title" title) <>
--        el "body" (getStructureString content)))

-- -- 實際產生的 HTML 內容
-- myhtml :: Html
-- myhtml =
--   html_
--     "My title"
--     ( append_
--         (h1_ "Heading")
--         ( append_
--             (p_ "Paragraph #1")
--             (p_ "Paragraph #2")
--         )
--     )

-- -- 主程式
-- main :: IO ()
-- main = putStrLn (render myhtml)
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Html

-- myhtml :: Html
-- myhtml =
--   html_
--     "My title"
--     ( append_
--         (h1_ "Heading")
--         ( append_
--             (p_ "Paragraph #1")
--             (p_ "Paragraph #2")
--         )
--     )

-- main :: IO ()
-- main = putStrLn (render myhtml)
-- {-# LANGUAGE OverloadedStrings #-}

-- import Network.HTTP.Req
-- import Data.Aeson (Value)

-- main :: IO ()
-- main = runReq defaultHttpConfig $ do
--   let params = "latitude" =: (25.03 :: Double)
--             <> "longitude" =: (121.56 :: Double)
--   r <- req GET (https "api.open-meteo.com" /: "v1" /: "forecast")
--          NoReqBody jsonResponse params
--   let body = responseBody r  -- 🔸 去除類型註記
--   liftIO $ print (body :: Value)  -- ✅ 如有需要可在這裡指定型別
-- {-# LANGUAGE OverloadedStrings #-}

-- import Network.HTTP.Req
-- import Data.Aeson (Value)
-- import Control.Monad.IO.Class (liftIO)  -- 加上這行

-- main :: IO ()
-- main = runReq defaultHttpConfig $ do
--   let params = "latitude" =: (25.03 :: Double)
--             <> "longitude" =: (121.56 :: Double)
--             <> "current_weather" =: True
--   r <- req GET (https "api.open-meteo.com" /: "v1" /: "forecast")
--          NoReqBody jsonResponse params
--   let body = responseBody r
--   liftIO $ print (body :: Value)  -- 若需要可在這裡指定型別

-- module Main where

-- import Markdown
-- import Markup

-- main :: IO ()
-- main = do
--   content <- readFile "example.md"
--   let doc = parseMarkdown content
--   mapM_ print doc
-- module Main where

-- -- 定義 Person 資料型別
-- data Person = Person
--   { name :: String
--   , age  :: Int
--   } deriving (Show)

-- main :: IO ()
-- main = do
--   let person1 = Person "Gil" 32

--   -- 印出整個資料
--   print person1

--   -- 印出欄位
--   putStrLn $ "Name: " ++ name person1
--   putStrLn $ "Age: "  ++ show (age person1)

--   -- 修改欄位（產生新值）
--   let person2 = person1 { age = 33 }
--   print person2

-- Main.hs
-- module Main where

-- -- 定義一個泛型資料型別 Tuple
-- data Tuple a b = Tuple a b deriving (Show)

-- -- 自訂函式：交換 Tuple 中的元素
-- swap :: Tuple a b -> Tuple b a
-- swap (Tuple x y) = Tuple y x

-- main :: IO ()
-- main = do
--   -- 建立一個 Tuple 值
--   let t1 = Tuple "Clicked" True

--   -- 印出原始 Tuple
--   putStrLn "Original Tuple:"
--   print t1

--   -- 呼叫 swap 函式
--   let t2 = swap t1

--   -- 印出交換後的 Tuple
--   putStrLn "Swapped Tuple:"
--   print t2
-- module Main where




-- main :: IO ()
-- main = do
-- module Main where

-- -- 自訂一個布林型別
-- data MyBool = MyTrue | MyFalse

-- -- 為 MyBool 定義 Show 實例
-- instance Show MyBool where
--   show x =
--     case x of
--       MyTrue  -> "True"
--       MyFalse -> "False"

-- main :: IO ()
-- main = do
--   let a = MyTrue
--   let b = MyFalse
--   putStrLn (show a)
--   putStrLn (show b)

-- {-# LANGUAGE DeriveGeneric #-}

-- module Main where

-- import GHC.Generics (Generic)
-- import Data.Semigroup (Semigroup(..))

-- -- 自訂錯誤訊息容器
-- newtype ValidationErrors = VE [String]
--   deriving Show

-- -- 實作 Semigroup：讓錯誤訊息可以合併
-- instance Semigroup ValidationErrors where
--   (VE e1) <> (VE e2) = VE (e1 ++ e2)

-- -- 驗證函數：檢查名字是否為空
-- validateName :: String -> Either ValidationErrors String
-- validateName name =
--   if null name
--     then Left (VE ["Name is required"])
--     else Right name

-- -- 驗證函數：檢查年齡是否為正整數
-- validateAge :: Int -> Either ValidationErrors Int
-- validateAge age =
--   if age <= 0
--     then Left (VE ["Age must be greater than 0"])
--     else Right age

-- -- 合併兩個 Either 結果：錯誤時合併錯誤，正確時取其中一個
-- combineE :: Either ValidationErrors a -> Either ValidationErrors a -> Either ValidationErrors a
-- combineE (Left e1) (Left e2) = Left (e1 <> e2)
-- combineE (Left e) _ = Left e
-- combineE _ (Left e) = Left e
-- combineE (Right _) (Right x) = Right x

-- -- 主程式
-- main :: IO ()
-- main = do
--   let nameInput = ""      -- 錯誤：空字串
--   let ageInput = (-3)     -- 錯誤：小於等於 0

--   let result = combineE (validateName nameInput) (validateAge ageInput)

--   case result of
--     Right _ -> putStrLn "Validation succeeded!"
--     Left (VE errs) -> do
--       putStrLn "Validation failed with the following errors:"
--       mapM_ putStrLn errs
-- {-# LANGUAGE DeriveGeneric #-}

-- module Main where

-- import GHC.Generics (Generic)
-- import Data.Semigroup (Semigroup(..))

-- -- 自訂錯誤訊息容器
-- newtype ValidationErrors = VE [String]
--   deriving Show

-- -- 實作 Semigroup：讓錯誤訊息可以合併
-- instance Semigroup ValidationErrors where
--   (VE e1) <> (VE e2) = VE (e1 ++ e2)

-- -- 驗證函數：檢查名字是否為空
-- validateName :: String -> Either ValidationErrors ()
-- validateName name =
--   if null name
--     then Left (VE ["Name is required"])
--     else Right ()

-- -- 驗證函數：檢查年齡是否為正整數
-- validateAge :: Int -> Either ValidationErrors ()
-- validateAge age =
--   if age <= 0
--     then Left (VE ["Age must be greater than 0"])
--     else Right ()

-- -- 驗證函數：檢查 Email 格式（是否含有 @）
-- validateEmail :: String -> Either ValidationErrors ()
-- validateEmail email =
--   if '@' `elem` email
--     then Right ()
--     else Left (VE ["Email format is invalid"])

-- -- 合併多個驗證錯誤：只收集錯誤，不管成功值
-- mergeErrors :: [Either ValidationErrors ()] -> Either ValidationErrors ()
-- mergeErrors = foldr combine (Right ())
--   where
--     combine (Left e1) (Left e2) = Left (e1 <> e2)
--     combine (Left e) _ = Left e
--     combine _ (Left e) = Left e
--     combine _ _ = Right ()

-- -- 主程式
-- main :: IO ()
-- main = do
--   let nameInput =  ""          -- 錯誤：名字空白
--   let ageInput = (-3)          -- 錯誤：年齡小於等於 0
--   let emailInput = "invalid"   -- 錯誤：沒有 @

--   let result = mergeErrors
--         [ validateName nameInput
--         , validateAge ageInput
--         , validateEmail emailInput
--         ]

--   case result of
--     Right _ -> putStrLn "Validation succeeded!"
--     Left (VE errs) -> do
--       putStrLn "Validation failed with the following errors:"
--       mapM_ putStrLn errs
module Main where

-- 自訂一個簡單的 log 型別
newtype LogMessage = Log String
  deriving Show

-- 實作 Semigroup：定義如何合併兩個 LogMessage
instance Semigroup LogMessage where
  Log a <> Log b = Log (a ++ "\n" ++ b)

-- 實作 Monoid：定義「空 log」是什麼
instance Monoid LogMessage where
  mempty = Log ""  -- 空字串為單位元

-- 測試主程式
main :: IO ()
main = do
  let logs = [Log "啟動系統", Log "載入設定", Log "完成初始化"]
  let combined = mconcat logs
  putStrLn " 合併後的日誌："
  print combined

-- x :: Int
-- x = 3
-- x = 9
-- y :: Int
-- y = y + 1
-- main :: IO ()
-- main = do
--     putStrLn "Hello, Haskell!"
--     putStrLn $ "Max Int: " ++ show biggestInt
--     putStrLn $ "Min Int: " ++ show smallestInt

-- biggestInt, smallestInt :: Int
-- biggestInt  = maxBound  -- Int 的最大值
-- smallestInt = minBound  -- Int 的最小值

-- -- 計算從 1 到 n 的總和
-- 計算從 1 到 n 的總和
-- sumtorial :: Integer -> Integer  -- 指定函數類型：輸入和輸出都是 Integer
-- sumtorial 0 = 0                  -- 終止條件：當 n 為 0 時，回傳 0
-- sumtorial n = n + sumtorial (n-1)  -- 遞迴計算 n + (n-1) 的總和
-- grade :: Int -> String
-- grade score
--   | score >= 90 = "A"
--   | score >= 80 = "B"
--   | score >= 70 = "C"
--   | score >= 60 = "D"
--   | otherwise = "F"
-- isEven :: Integer -> Bool
-- isEven n
--   | n `mod` 2 == 0 = True
--   | otherwise = False
-- isEven :: Integer -> Bool
-- isEven n = mod n 2 == 0
-- 這是一個更靈活的 filter 版本，允許你動態傳入不同的過濾函數
-- filterWithFunction :: (a -> Bool) -> [a] -> [a]
-- filterWithFunction _ [] = []
-- filterWithFunction pred (x:xs)
--   | pred x    = x : filterWithFunction pred xs  -- 若條件符合，保留元素
--   | otherwise = filterWithFunction pred xs      -- 否則丟棄
-- isEven :: Int -> Bool
-- isEven x = x `mod` 2 == 0

-- greaterThanFive :: Int -> Bool
-- greaterThanFive x = x > 5

-- result4 = filterWithFunction isEven [1,2,3,4,5,6,7,8] -- [2,4,6,8]
-- result5 = filterWithFunction greaterThanFive [1,2,3,4,5,6,7,8] -- [6,7,8]
-- applyFunctionsAlternately :: (a -> a) -> (a -> a) -> [a] -> [a]
-- applyFunctionsAlternately _ _ [] = []
-- applyFunctionsAlternately f g (x:xs) =
--     f x : applyFunctionsAlternately g f xs  -- 交替應用 f 和 g
-- increment x = x + 1
-- decrement x = x - 1

-- result6 = applyFunctionsAlternately increment decrement [1,2,3,4,5]
-- 計算結果：[2,1,4,3,6]

-- data Thing = Shoe
--            | Ship
--            | SealingWax
--            | Cabbage
--            | King
--   deriving Show
-- shoe :: Thing
-- shoe = Shoe
-- listO'Things :: [Thing]
-- listO'Things = [Shoe, SealingWax, King, Cabbage, King]

-- isSmall :: Thing -> Bool
-- isSmall Shoe       = True
-- isSmall Ship       = False
-- isSmall SealingWax = True
-- isSmall Cabbage    = True
-- isSmall King       = False


-- isSmall2 :: Thing -> Bool
-- isSmall2 Ship = False
-- isSmall2 King = False
-- isSmall2 _    = True


-- data FailableDouble = Failure
--                     | OK Double
--   deriving Show

-- safeDiv :: Double -> Double -> FailableDouble
-- safeDiv _ 0 = Failure
-- safeDiv x y = OK (x / y)
-- failureToZero :: FailableDouble -> Double
-- failureToZero Failure  = 0
-- failureToZero (OK d)   = d


-- data Type11 = A | B deriving Show
-- data Type12 = X | Y deriving Show
-- data Type21 = Yes | No deriving Show
-- data Type31 = Red | Blue deriving Show
-- data Type32 = Circle | Square deriving Show
-- data Type33 = Cat | Dog deriving Show


-- data AlgDataType = Constr1 Type11 Type12
--                  | Constr2 Type21
--                  | Constr3 Type31 Type32 Type33
--                  | Constr4
--   deriving Show

-- data JSON = JSONNumber Double
--           | JSONBool Bool
--           | JSONString String
--           | JSONArray [JSON]
--           | JSONObject [(String, JSON)]
--           | JSONNull
--   deriving Show
-- user :: JSON
-- user = JSONObject 
--   [ ("firstName", JSONString "John")
--   , ("lastName", JSONString "Smith")
--   , ("sex", JSONString "male")
--   , ("age", JSONNumber 25)
--   , ("address", JSONObject 
--       [ ("streetAddress", JSONString "21 2nd Street")
--       , ("city", JSONString "New York")
--       , ("state", JSONString "NY")
--       , ("postalCode", JSONString "10021")
--       ])
--   , ("phoneNumber", JSONArray 
--       [ JSONObject 
--           [ ("type", JSONString "home")
--           , ("number", JSONString "212 555-1234")
--           ]
--       , JSONObject 
--           [ ("type", JSONString "fax")
--           , ("number", JSONString "646 555-4567")
--           ]
--       ])
--   ]
-- getField :: String -> JSON -> Maybe JSON
-- getField key (JSONObject obj) = lookup key obj
-- getField _ _ = Nothing
-- getUserName :: JSON -> Maybe String
-- getUserName json = do
--   JSONString firstName <- getField "firstName" json
--   JSONString lastName  <- getField "lastName" json
--   return (firstName ++ " " ++ lastName)

-- -- 測試：
-- getUserName user  -- Just "John Smith"

-- data Maybe12 a = Just12 a | Nothing12
--   deriving Show
-- getValue :: Maybe12 a -> String
-- getValue (Just12 _) = "Has a value"
-- getValue Nothing12  = "No value"

-- safeDiv :: Double -> Double -> Maybe12 Double
-- safeDiv _ 0 = Nothing12
-- safeDiv x y = Just12 (x / y)
-- data Person = Person String Int String
--   deriving Show
-- -- checkAge :: Person -> String
-- -- checkAge (Person _ age _) = "Age is " ++ show age
-- baz :: Person -> String
-- baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- pat ::= _
--       | var
--       | var @ (pat)
--       | (Constructor pat1 pat2 ... patn)
-- foo :: Int -> String
-- foo _ = "Any number is fine!"
-- foo :: Int -> String
-- foo x = "The number is " ++ show x
-- data IntList = Empty           -- 空列表
--              | Cons Int IntList -- 一個 Int + 剩餘的 IntList

-- -- 手動實作 Show
-- instance Show IntList where
--   show = showIntList

-- showIntList :: IntList -> String
-- showIntList Empty = "[]"
-- showIntList (Cons x xs) = "[" ++ show x ++ listTail xs ++ "]"
--   where
--     listTail Empty = ""
--     listTail (Cons y ys) = ", " ++ show y ++ listTail ys

-- -- 定義一些操作
-- absAll :: IntList -> IntList
-- absAll Empty        = Empty
-- absAll (Cons x xs)  = Cons (abs x) (absAll xs)

-- squareAll :: IntList -> IntList
-- squareAll Empty        = Empty
-- squareAll (Cons x xs)  = Cons (x * x) (squareAll xs)

-- mapIntList :: (Int -> Int) -> IntList -> IntList
-- mapIntList _ Empty        = Empty
-- mapIntList f (Cons x xs)  = Cons (f x) (mapIntList f xs)

-- -- 建立一個測試用的 IntList
-- exampleList :: IntList
-- exampleList = Cons 1 (Cons 2 (Cons 3 Empty))
-- keepOnlyEven :: IntList -> IntList
-- keepOnlyEven Empty = Empty
-- keepOnlyEven (Cons x xs)
--   | even x    = Cons x (keepOnlyEven xs)  -- 保留偶數
--   | otherwise = keepOnlyEven xs           -- 丟棄奇數
-- filterIntList :: (Int -> Bool) -> IntList -> IntList
-- filterIntList _ Empty = Empty
-- filterIntList f (Cons x xs)
--   | f x       = Cons x (filterIntList f xs)  -- 只保留符合 `f x` 的元素
--   | otherwise = filterIntList f xs           -- 丟棄不符合的元素

-- data List t = E                 -- 空列表
--             | C t (List t)       -- 包含一個 `t` 值和剩餘的 `List t`


-- instance Show t => Show (List t) where
--   show E = "[]"
--   show (C x xs) = "[" ++ show x ++ listTail xs ++ "]"
--     where
--       listTail E = ""
--       listTail (C y ys) = ", " ++ show y ++ listTail ys

-- -- 測試用的 List
-- lst1 :: List Int
-- lst1 = C 3 (C 5 (C 2 E))

-- lst2 :: List Char
-- lst2 = C 'x' (C 'y' (C 'z' E))

-- lst3 :: List Bool
-- lst3 = C True (C False E)
-- gt100 :: Integer -> Bool
-- gt100 x = x > 100

-- greaterThan100 :: [Integer] -> [Integer]
-- greaterThan100 xs = filter gt100 xs
-- f :: Num a => a -> a -> a
-- f x = \y -> (x + y)
-- {-# LANGUAGE ExistentialQuantification #-}

-- newtype ShowableFunction a b = ShowableFunction (a -> b)

-- instance Show (ShowableFunction a b) where
--     show _ = "<function>"
-- f :: Num a => a -> ShowableFunction a a
-- f x = ShowableFunction (\y -> x + y)double = (*2)
-- increment = (+1)

-- double :: Integer -> Integer
-- double = (*2)
-- increment :: Integer -> Integer
-- increment = (+1)
-- add :: Int -> Int -> Int
-- add x y = x + y

-- add5 :: Int -> Int
-- add5 = add 5
-- myid :: a -> a
-- myid x = x
-- module Main where

-- -- 1. 純函式：沒有副作用，只做計算
-- pureAdd :: Int -> Int -> Int
-- pureAdd x y = x + y

-- -- 2. 主程式：負責與使用者互動 (IO)
-- main :: IO ()
-- main = do
--     putStrLn "請輸入第一個數字:"
--     x <- readLn             -- 從標準輸入讀取數字 (副作用)
--     putStrLn "請輸入第二個數字:"
--     y <- readLn             -- 再次讀取數字 (副作用)

--     let result = pureAdd x y  -- 呼叫純函式做計算
--     putStrLn ("結果是: " ++ show result) -- 印出結果 (副作用)
-- module Main where

-- main :: IO ()
-- main = do
--     -- 這裡定義一個非常大的運算：29^35792
--     -- 在嚴格語言中，這一行就會先把 29^35792 算出來（非常耗時）。
--     -- 但在 Haskell 中，它只是一個 thunk，尚未計算。
--     let bigNumber = 29 ^ 35792

--     putStrLn "已經宣告 bigNumber = 29^35792，但尚未使用它。"
--     putStrLn "按 Enter 鍵繼續..."
--     _ <- getLine

--     -- 這裡並沒有用到 bigNumber，因此它永遠不會被真正計算。
--     putStrLn "我們完全沒用到 bigNumber，程式將直接結束。"
--     putStrLn "按 Enter 鍵退出..."
  
-- safeHead :: [a] -> Maybe a
-- safeHead []    = Nothing
-- safeHead (x:_) = Just x

-- describe :: Maybe Int -> String
-- describe Nothing  = "No value"
-- describe (Just x) = "The value is " ++ show x
-- f1 :: Maybe a -> [Maybe a]
-- f1 m = [m, m]
-- import Prelude hiding (repeat, take)
-- repeat :: a -> [a]
-- repeat x = x : repeat x
-- take :: Int -> [a] -> [a]
-- take n _  | n <= 0 = []
-- take _ [] = []
-- take n (x:xs) = x : take (n-1) xs
-- data ExprT = Lit Integer
--            | Add ExprT ExprT
--            | Mul ExprT ExprT
--            deriving (Show)

-- data JoinList m a = Empty
--                   | Single m a
--                   | Append m (JoinList m a) (JoinList m a)

-- import Prelude hiding (Functor, fmap)

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- instance Functor Maybe where
--     fmap _ Nothing  = Nothing
--     fmap h (Just a) = Just (h a)
-- instance Functor [] where
--   fmap _ []     = []
--   fmap f (x:xs) = f x : fmap f xs

-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Crypto.Random (getRandomBytes)  -- 生成隨機數
-- import Data.ByteString (ByteString)     -- 二進制數據
-- import qualified Data.ByteString as BS
-- import Crypto.Hash (SHA3_256, hash)     -- 使用 SHA3-256 作為哈希函數
-- import Data.Bits (xor)                  -- 位運算（異或操作）
-- import Data.List (foldl')

-- -- Kyber 參數
-- n :: Int
-- n = 256  -- Kyber 主要使用 n=256 的環

-- q :: Int
-- q = 3329  -- Kyber 使用的素數模數

-- -- 生成 n 個隨機係數（代表多項式）
-- generatePoly :: IO [Int]
-- generatePoly = do
--     bytes <- getRandomBytes n
--     return $ map (`mod` q) (BS.unpack bytes)  -- 確保係數在模 q 的範圍內

-- -- 多項式加法 (模 q)
-- polyAdd :: [Int] -> [Int] -> [Int]
-- polyAdd a b = zipWith (\x y -> (x + y) `mod` q) a b

-- -- 多項式減法 (模 q)
-- polySub :: [Int] -> [Int] -> [Int]
-- polySub a b = zipWith (\x y -> (x - y) `mod` q) a b

-- -- 多項式點乘 (模 q)
-- polyMul :: [Int] -> [Int] -> [Int]
-- polyMul a b = take n $ foldl' (\acc (i, ai) -> zipWith (+) acc (shiftPoly (map (* ai) b) i)) (replicate n 0) (zip [0..] a)
--   where
--     shiftPoly :: [Int] -> Int -> [Int]
--     shiftPoly p s = replicate s 0 ++ p ++ replicate (n - length p - s) 0

-- -- 生成密鑰對
-- generateKeyPair :: IO ([Int], [Int])
-- generateKeyPair = do
--     sk <- generatePoly  -- 私鑰 (隨機多項式)
--     e <- generatePoly   -- 雜訊
--     let pk = polyAdd sk e  -- 公鑰 pk = sk + e (簡化版)
--     return (pk, sk)

-- -- 加密
-- encrypt :: [Int] -> [Int] -> IO [Int]
-- encrypt publicKey message = do
--     r <- generatePoly  -- 生成隨機多項式 r
--     let u = polyMul publicKey r  -- u = pk * r
--     let v = polyAdd message r    -- v = msg + r
--     return (polyAdd u v)         -- ciphertext = u + v (簡化版)

-- -- 解密
-- decrypt :: [Int] -> [Int] -> [Int]
-- decrypt secretKey ciphertext =
--     polySub ciphertext secretKey  -- 恢復明文 m = c - sk

-- main :: IO ()
-- main = do
--     -- 生成密鑰對
--     (publicKey, secretKey) <- generateKeyPair

--     putStrLn "Generated Public Key:"
--     print publicKey

--     putStrLn "Generated Secret Key:"
--     print secretKey

--     -- 定義明文消息 (模擬多項式)
--     message <- generatePoly
--     putStrLn "Original Message:"
--     print message

--     -- 加密
--     ciphertext <- encrypt publicKey message
--     putStrLn "Encrypted Ciphertext:"
--     print ciphertext

--     -- 解密
--     decryptedMessage <- return (decrypt secretKey ciphertext)
--     putStrLn "Decrypted Message:"
--     print decryptedMessage

--     if message == decryptedMessage
--         then putStrLn "Decryption Successful!"
--         else putStrLn "Decryption Failed!"


-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Crypto.Random (getRandomBytes)
-- import Data.Bits (shiftL, shiftR, xor)
-- import Data.Word (Word16)
-- import Data.List (foldl')
-- import qualified Data.ByteString as BS
-- import System.Random (randomRIO)

-- -- Kyber 參數
-- q :: Int
-- q = 3329   -- Kyber 素數模數

-- ω :: Int
-- ω = 17     -- NTT 的原根

-- n :: Int
-- n = 256    -- 多項式大小

-- -- Kyber 參數集
-- data KyberParams = KyberParams { k :: Int }  -- k 代表多項式數組的大小

-- kyber512, kyber768, kyber1024 :: KyberParams
-- kyber512  = KyberParams { k = 2 }
-- kyber768  = KyberParams { k = 3 }
-- kyber1024 = KyberParams { k = 4 }

-- -- ==========================
-- -- 1️⃣ 離散高斯取樣 (DGS)
-- -- ==========================
-- boxMuller :: IO (Double, Double)
-- boxMuller = do
--     u1 <- randomRIO (0.0001, 1.0)
--     u2 <- randomRIO (0.0001, 1.0)
--     let r = sqrt (-2.0 * log u1)
--         theta = 2.0 * pi * u2
--         z1 = r * cos theta
--         z2 = r * sin theta
--     return (z1, z2)

-- discreteGaussian :: Double -> IO Int
-- discreteGaussian sigma = do
--     (z1, _) <- boxMuller
--     return $ round (z1 * sigma)

-- generateNoisePoly :: IO [Int]
-- generateNoisePoly = mapM (const (discreteGaussian 3.0)) [1..n]

-- -- ==========================
-- -- 2️⃣ Number Theoretic Transform (NTT)
-- -- ==========================
-- modExp :: Int -> Int -> Int -> Int
-- modExp base exp m = go base exp 1
--   where
--     go _ 0 res = res
--     go b e res
--       | e `mod` 2 == 1 = go ((b * b) `mod` m) (e `div` 2) ((res * b) `mod` m)
--       | otherwise = go ((b * b) `mod` m) (e `div` 2) res

-- ntt :: [Int] -> [Int]
-- ntt a = [sum [a !! j * modExp ω ((i * j) `mod` n) q | j <- [0..n-1]] `mod` q | i <- [0..n-1]]

-- intt :: [Int] -> [Int]
-- intt a = [sum [a !! j * modExp ω ((-i * j) `mod` n) q | j <- [0..n-1]] `mod` q | i <- [0..n-1]]

-- -- ==========================
-- -- 3️⃣ Kyber 密鑰生成
-- -- ==========================
-- generateKeyPair :: IO ([Int], [Int])
-- generateKeyPair = do
--     sk <- generateNoisePoly
--     e <- generateNoisePoly
--     let pk = ntt (zipWith (+) sk e)
--     return (pk, sk)

-- -- ==========================
-- -- 4️⃣ Kyber 加密
-- -- ==========================
-- encrypt :: [Int] -> [Int] -> IO [Int]
-- encrypt pk message = do
--     r <- generateNoisePoly
--     let u = ntt (zipWith (*) pk r)
--         v = zipWith (+) message r
--     return (zipWith (+) u v)

-- -- ==========================
-- -- 5️⃣ Kyber 解密
-- -- ==========================
-- decrypt :: [Int] -> [Int] -> [Int]
-- decrypt sk ciphertext = intt (zipWith (-) ciphertext sk)

-- -- ==========================
-- -- 6️⃣ 測試 Kyber-512
-- -- ==========================
-- main :: IO ()
-- main = do
--     let params = kyber512  -- 選擇 Kyber-512

--     -- 生成密鑰對
--     (publicKey, secretKey) <- generateKeyPair

--     putStrLn "Generated Public Key:"
--     print publicKey

--     putStrLn "Generated Secret Key:"
--     print secretKey

--     -- 定義明文
--     message <- generateNoisePoly
--     putStrLn "Original Message:"
--     print message

--     -- 加密
--     ciphertext <- encrypt publicKey message
--     putStrLn "Encrypted Ciphertext:"
--     print ciphertext

--     -- 解密
--     decryptedMessage <- return (decrypt secretKey ciphertext)
--     putStrLn "Decrypted Message:"
--     print decryptedMessage

--     if message == decryptedMessage
--         then putStrLn "Decryption Successful!"
--         else putStrLn "Decryption Failed!"
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Crypto.Random (getRandomBytes)
-- import Data.Bits (shiftL, shiftR, xor)
-- import Data.Word (Word16)
-- import Data.List (foldl')
-- import qualified Data.ByteString as BS
-- import System.Random (randomRIO)

-- -- Kyber 參數
-- q :: Int
-- q = 3329   -- Kyber 素數模數

-- ω :: Int
-- ω = 17     -- NTT 的原根

-- n :: Int
-- n = 256    -- 多項式大小

-- -- ==========================
-- -- 1️⃣ 修正中心化縮減 (Centered Reduction)
-- -- ==========================
-- centeredMod :: Int -> Int -> Int
-- centeredMod x m = let r = x `mod` m in if r > m `div` 2 then r - m else r

-- -- ==========================
-- -- 2️⃣ 離散高斯取樣 (DGS)
-- -- ==========================
-- boxMuller :: IO (Double, Double)
-- boxMuller = do
--     u1 <- randomRIO (0.0001, 1.0)
--     u2 <- randomRIO (0.0001, 1.0)
--     let r = sqrt (-2.0 * log u1)
--         theta = 2.0 * pi * u2
--         z1 = r * cos theta
--         z2 = r * sin theta
--     return (z1, z2)

-- discreteGaussian :: Double -> IO Int
-- discreteGaussian sigma = do
--     (z1, _) <- boxMuller
--     return $ centeredMod (round (z1 * sigma)) q

-- generateNoisePoly :: IO [Int]
-- generateNoisePoly = mapM (const (discreteGaussian 3.0)) [1..n]

-- -- ==========================
-- -- 3️⃣ 修正 NTT 變換
-- -- ==========================
-- modExp :: Int -> Int -> Int -> Int
-- modExp base exp m = go base exp 1
--   where
--     go _ 0 res = res
--     go b e res
--       | e `mod` 2 == 1 = go ((b * b) `mod` m) (e `div` 2) ((res * b) `mod` m)
--       | otherwise = go ((b * b) `mod` m) (e `div` 2) res

-- ntt :: [Int] -> [Int]
-- ntt a = [sum [a !! j * modExp ω ((i * j) `mod` n) q | j <- [0..n-1]] `mod` q | i <- [0..n-1]]

-- intt :: [Int] -> [Int]
-- intt a = [centeredMod (sum [a !! j * modExp ω ((-i * j) `mod` n) q | j <- [0..n-1]] `mod` q) q | i <- [0..n-1]]

-- -- ==========================
-- -- 4️⃣ 修正 Kyber 密鑰生成
-- -- ==========================
-- generateKeyPair :: IO ([Int], [Int])
-- generateKeyPair = do
--     sk <- generateNoisePoly
--     e <- generateNoisePoly
--     let pk = ntt (zipWith (+) sk e)  -- 修正範圍
--     return (pk, sk)

-- -- ==========================
-- -- 5️⃣ 修正加密
-- -- ==========================
-- encrypt :: [Int] -> [Int] -> IO [Int]
-- encrypt pk message = do
--     r <- generateNoisePoly
--     let u = ntt (zipWith (*) pk r)
--         v = zipWith (\m r -> centeredMod (m + r) q) message r  -- 修正範圍
--     return (zipWith (\x y -> centeredMod (x + y) q) u v)

-- -- ==========================
-- -- 6️⃣ 修正解密
-- -- ==========================
-- decrypt :: [Int] -> [Int] -> [Int]
-- decrypt sk ciphertext = intt (zipWith (\c s -> centeredMod (c - s) q) ciphertext sk)

-- -- ==========================
-- -- 7️⃣ 測試 Kyber-512
-- -- ==========================
-- main :: IO ()
-- main = do
--     -- 生成密鑰對
--     (publicKey, secretKey) <- generateKeyPair

--     putStrLn "Generated Public Key:"
--     print publicKey

--     putStrLn "Generated Secret Key:"
--     print secretKey

--     -- 定義明文
--     message <- generateNoisePoly
--     putStrLn "Original Message:"
--     print message

--     -- 加密
--     ciphertext <- encrypt publicKey message
--     putStrLn "Encrypted Ciphertext:"
--     print ciphertext

--     -- 解密
--     decryptedMessage <- return (decrypt secretKey ciphertext)
--     putStrLn "Decrypted Message:"
--     print decryptedMessage

--     if message == decryptedMessage
--         then putStrLn "✅ Decryption Successful!"
--         else putStrLn "❌ Decryption Failed!"
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Crypto.Random (getRandomBytes)
-- import Data.Bits (shiftL, shiftR, xor)
-- import Data.Word (Word16)
-- import Data.List (foldl')
-- import qualified Data.ByteString as BS
-- import System.Random (randomRIO)

-- -- Kyber 參數
-- q :: Int
-- q = 3329   -- Kyber 素數模數

-- ω :: Int
-- ω = 17     -- NTT 的原根

-- n :: Int
-- n = 256    -- 多項式大小

-- -- ==========================
-- -- 1️⃣ 修正中心化縮減 (Centered Reduction)
-- -- ==========================
-- centeredMod :: Int -> Int -> Int
-- centeredMod x m = let r = x `mod` m in if r > m `div` 2 then r - m else r

-- -- ==========================
-- -- 2️⃣ 修正離散高斯取樣 (DGS)
-- -- ==========================
-- boxMuller :: IO (Double, Double)
-- boxMuller = do
--     u1 <- randomRIO (0.0001, 1.0)
--     u2 <- randomRIO (0.0001, 1.0)
--     let r = sqrt (-2.0 * log u1)
--         theta = 2.0 * pi * u2
--         z1 = r * cos theta
--         z2 = r * sin theta
--     return (z1, z2)

-- discreteGaussian :: Double -> IO Int
-- discreteGaussian sigma = do
--     (z1, _) <- boxMuller
--     return $ centeredMod (round (z1 * sigma)) q

-- generateNoisePoly :: IO [Int]
-- generateNoisePoly = mapM (const (discreteGaussian 3.19)) [1..n]

-- -- ==========================
-- -- 3️⃣ 修正 NTT 變換
-- -- ==========================
-- modExp :: Int -> Int -> Int -> Int
-- modExp base exp m = go base exp 1
--   where
--     go _ 0 res = res
--     go b e res
--       | e `mod` 2 == 1 = go ((b * b) `mod` m) (e `div` 2) ((res * b) `mod` m)
--       | otherwise = go ((b * b) `mod` m) (e `div` 2) res

-- ntt :: [Int] -> [Int]
-- ntt a = [sum [a !! j * modExp ω ((i * j) `mod` n) q | j <- [0..n-1]] `mod` q | i <- [0..n-1]]

-- intt :: [Int] -> [Int]
-- intt a = [centeredMod (sum [a !! j * modExp ω ((-i * j) `mod` n) q | j <- [0..n-1]] `mod` q) q | i <- [0..n-1]]

-- -- ==========================
-- -- 4️⃣ 修正 Kyber 密鑰生成
-- -- ==========================
-- generateKeyPair :: IO ([Int], [Int])
-- generateKeyPair = do
--     sk <- generateNoisePoly
--     e <- generateNoisePoly
--     let pk = ntt (zipWith (\s e -> centeredMod (s + e) q) sk e)  -- 修正範圍
--     return (pk, sk)

-- -- ==========================
-- -- 5️⃣ 修正加密
-- -- ==========================
-- encrypt :: [Int] -> [Int] -> IO [Int]
-- encrypt pk message = do
--     r <- generateNoisePoly
--     let u = ntt (zipWith (\p r -> centeredMod (p * r) q) pk r)
--         v = zipWith (\m r -> centeredMod (m + r) q) message r  -- 修正範圍
--     return (zipWith (\x y -> centeredMod (x + y) q) u v)

-- -- ==========================
-- -- 6️⃣ 修正解密
-- -- ==========================
-- decrypt :: [Int] -> [Int] -> [Int]
-- decrypt sk ciphertext = intt (zipWith (\c s -> centeredMod (c - s) q) ciphertext sk)

-- -- ==========================
-- -- 7️⃣ 修正字符輸出，避免無法解析的符號
-- -- ==========================
-- filterValidChars :: [Int] -> String
-- filterValidChars xs = map (\x -> if x >= 32 && x <= 126 then toEnum x else '?') (map (`mod` 128) xs)

-- -- ==========================
-- -- 8️⃣ 測試 Kyber-512
-- -- ==========================
-- main :: IO ()
-- main = do
--     -- 生成密鑰對
--     (publicKey, secretKey) <- generateKeyPair

--     putStrLn "Generated Public Key:"
--     print publicKey

--     putStrLn "Generated Secret Key:"
--     print secretKey

--     -- 定義明文
--     message <- generateNoisePoly
--     putStrLn "Original Message:"
--     print message
--     putStrLn "Original Message (ASCII Safe):"
--     putStrLn (filterValidChars message)

--     -- 加密
--     ciphertext <- encrypt publicKey message
--     putStrLn "Encrypted Ciphertext:"
--     print ciphertext

--     -- 解密
--     decryptedMessage <- return (decrypt secretKey ciphertext)
--     putStrLn "Decrypted Message:"
--     print decryptedMessage
--     putStrLn "Decrypted Message (ASCII Safe):"
--     putStrLn (filterValidChars decryptedMessage)

--     if message == decryptedMessage
--         then putStrLn "✅ Decryption Successful!"
--         else putStrLn "❌ Decryption Failed!"


-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Crypto.Random (getRandomBytes)
-- import Data.Bits (shiftL, shiftR, xor)
-- import Data.Word (Word16)
-- import Data.List (foldl')
-- import qualified Data.ByteString as BS
-- import System.Random (randomRIO)

-- -- Kyber 參數
-- q :: Int
-- q = 3329   -- Kyber 素數模數

-- omega :: Int
-- omega = 17     -- NTT 的原根

-- n :: Int
-- n = 256    -- 多項式大小

-- -- 256 在模 3329 下的逆元素 (由擴展歐幾里得算法可得)
-- inv_n :: Int
-- inv_n = 3316

-- -- ==========================
-- -- 1️⃣ 修正中心化縮減 (Centered Reduction)
-- -- ==========================
-- centeredMod :: Int -> Int -> Int
-- centeredMod x m = let r = x `mod` m in if r > m `div` 2 then r - m else r

-- -- ==========================
-- -- 2️⃣ 離散高斯取樣 (Discrete Gaussian Sampling)
-- -- ==========================
-- boxMuller :: IO (Double, Double)
-- boxMuller = do
--     u1 <- randomRIO (0.0001, 1.0)
--     u2 <- randomRIO (0.0001, 1.0)
--     let r = sqrt (-2.0 * log u1)
--         theta = 2.0 * pi * u2
--         z1 = r * cos theta
--         z2 = r * sin theta
--     return (z1, z2)

-- discreteGaussian :: Double -> IO Int
-- discreteGaussian sigma = do
--     (z1, _) <- boxMuller
--     -- 將結果縮減到模 q 範圍內
--     return $ centeredMod (round (z1 * sigma)) q

-- generateNoisePoly :: IO [Int]
-- generateNoisePoly = mapM (const (discreteGaussian 3.19)) [1..n]

-- -- ==========================
-- -- 3️⃣ NTT 變換及逆 NTT（加上縮放因子）
-- -- ==========================
-- modExp :: Int -> Int -> Int -> Int
-- modExp base exp m = go base exp 1
--   where
--     go _ 0 res = res
--     go b e res
--       | e `mod` 2 == 1 = go ((b * b) `mod` m) (e `div` 2) ((res * b) `mod` m)
--       | otherwise = go ((b * b) `mod` m) (e `div` 2) res

-- -- 前向 NTT
-- ntt :: [Int] -> [Int]
-- ntt a = [ sum [ (a !! j) * modExp omega ((i * j) `mod` n) q | j <- [0..n-1] ] `mod` q
--         | i <- [0..n-1] ]

-- -- 逆 NTT：計算完後再乘上 inv_n 以還原
-- intt :: [Int] -> [Int]
-- intt a = [ centeredMod ((s * inv_n) `mod` q) q
--          | i <- [0..n-1]
--          , let s = sum [ (a !! j) * modExp omega (((-i) * j) `mod` n) q | j <- [0..n-1] ] `mod` q
--          ]

-- -- ==========================
-- -- 4️⃣ Kyber 密鑰生成
-- -- ==========================
-- generateKeyPair :: IO ([Int], [Int])
-- generateKeyPair = do
--     sk <- generateNoisePoly
--     e  <- generateNoisePoly
--     let pk = ntt (zipWith (\s e -> centeredMod (s + e) q) sk e)
--     return (pk, sk)

-- -- ==========================
-- -- 5️⃣ 加密（簡化版）
-- -- ==========================
-- encrypt :: [Int] -> [Int] -> IO [Int]
-- encrypt pk message = do
--     r <- generateNoisePoly
--     let u = ntt (zipWith (\p r -> centeredMod (p * r) q) pk r)
--         v = zipWith (\m r -> centeredMod (m + r) q) message r
--     return (zipWith (\x y -> centeredMod (x + y) q) u v)

-- -- ==========================
-- -- 6️⃣ 解密（簡化版）
-- -- ==========================
-- decrypt :: [Int] -> [Int] -> [Int]
-- decrypt sk ciphertext = intt (zipWith (\c s -> centeredMod (c - s) q) ciphertext sk)

-- -- ==========================
-- -- 7️⃣ 過濾 ASCII 字符（避免輸出非法 Unicode 字元）
-- -- ==========================
-- filterValidChars :: [Int] -> String
-- filterValidChars xs = map (\x -> if x >= 32 && x <= 126 then toEnum x else '?') (map (`mod` 128) xs)

-- -- ==========================
-- -- 8️⃣ 主程式: 測試 Kyber-512
-- -- ==========================
-- main :: IO ()
-- main = do
--     -- 生成密鑰對
--     (publicKey, secretKey) <- generateKeyPair
--     putStrLn "Generated Public Key:"
--     print publicKey
--     putStrLn "Generated Secret Key:"
--     print secretKey

--     -- 定義明文（使用離散高斯取樣生成的多項式）
--     message <- generateNoisePoly
--     putStrLn "Original Message:"
--     print message
--     putStrLn "Original Message (ASCII Safe):"
--     putStrLn (filterValidChars message)

--     -- 加密
--     ciphertext <- encrypt publicKey message
--     putStrLn "Encrypted Ciphertext:"
--     print ciphertext

--     -- 解密
--     let decryptedMessage = decrypt secretKey ciphertext
--     putStrLn "Decrypted Message:"
--     print decryptedMessage
--     putStrLn "Decrypted Message (ASCII Safe):"
--     putStrLn (filterValidChars decryptedMessage)

--     if message == decryptedMessage
--       then putStrLn "Decryption Successful!"
--       else putStrLn "Decryption Failed!"

-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import System.Random (randomRIO)

-- -- Kyber 簡化版本參數
-- q :: Int
-- q = 3329  -- 模數（Kyber 使用的素數）

-- n :: Int
-- n = 256   -- 多項式長度

-- ----------------------------------------------------------
-- -- 1. 生成多項式
-- -- 生成一個長度為 n 的多項式，每個係數在 0 ~ (q-1) 之間隨機取值
-- ----------------------------------------------------------
-- generatePoly :: IO [Int]
-- generatePoly = mapM (\_ -> randomRIO (0, q - 1)) [1..n]

-- ----------------------------------------------------------
-- -- 2. 多項式加法
-- -- 將兩個多項式對應項相加後對 q 取模
-- ----------------------------------------------------------
-- polyAdd :: [Int] -> [Int] -> [Int]
-- polyAdd xs ys = zipWith (\x y -> (x + y) `mod` q) xs ys

-- ----------------------------------------------------------
-- -- 3. 密鑰生成
-- -- 生成密鑰對：私鑰 sk 為一個隨機多項式
-- -- 公鑰 pk 為私鑰加上一個雜訊多項式（簡化版本中僅用加法）
-- ----------------------------------------------------------
-- generateKeyPair :: IO ([Int], [Int])
-- generateKeyPair = do
--     sk  <- generatePoly          -- 生成私鑰多項式
--     err <- generatePoly          -- 生成雜訊多項式
--     let pk = polyAdd sk err      -- 公鑰 = 私鑰 + 雜訊
--     return (pk, sk)

-- ----------------------------------------------------------
-- -- 4. 加密
-- -- 輸入：公鑰 pk 和明文 message（均為多項式）
-- -- 隨機生成一個雜訊多項式，然後計算：
-- --    ciphertext = pk + message + noise
-- ----------------------------------------------------------
-- encrypt :: [Int] -> [Int] -> IO [Int]
-- encrypt pk message = do
--     noise <- generatePoly         -- 生成額外的雜訊
--     let ciphertext = polyAdd (polyAdd pk noise) message
--     return ciphertext

-- ----------------------------------------------------------
-- -- 5. 解密
-- -- 輸入：私鑰 sk 和密文 ciphertext
-- -- 解密時假設 ciphertext = (sk + err + noise) + message，
-- -- 故用密文減去私鑰即可還原出明文（在此版本中忽略雜訊衝突）
-- ----------------------------------------------------------
-- decrypt :: [Int] -> [Int] -> [Int]
-- decrypt sk ciphertext = zipWith (\c s -> (c - s) `mod` q) ciphertext sk

-- ----------------------------------------------------------
-- -- 6. 主程式：測試整個流程
-- ----------------------------------------------------------
-- main :: IO ()
-- main = do
--     -- 生成密鑰對
--     (pk, sk) <- generateKeyPair
--     putStrLn "Public Key:"
--     print pk
--     putStrLn "Secret Key:"
--     print sk

--     -- 生成一個明文多項式（在實際應用中，明文會先進行編碼）
--     message <- generatePoly
--     putStrLn "Original Message:"
--     print message

--     -- 加密：使用公鑰和明文生成密文
--     ciphertext <- encrypt pk message
--     putStrLn "Ciphertext:"
--     print ciphertext

--     -- 解密：用私鑰還原出明文
--     let decrypted = decrypt sk ciphertext
--     putStrLn "Decrypted Message:"
--     print decrypted

--     -- 驗證解密結果是否正確
--     if message == decrypted
--       then putStrLn "Decryption Successful!"
--       else putStrLn "Decryption Failed!"
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import System.Random (randomRIO)

-- -- 模數與多項式長度
-- q :: Int
-- q = 3329      -- 模數：Kyber 使用的素數
-- n :: Int
-- n = 256       -- 多項式長度

-- ----------------------------------------------------
-- -- 1. 生成多項式
-- -- 隨機生成一個長度為 n，每個係數在 [0, q-1] 的多項式
-- ----------------------------------------------------
-- generatePoly :: IO [Int]
-- generatePoly = mapM (\_ -> randomRIO (0, q - 1)) [1..n]

-- ----------------------------------------------------
-- -- 2. 多項式加法
-- -- 對應項相加後對 q 取模
-- ----------------------------------------------------
-- polyAdd :: [Int] -> [Int] -> [Int]
-- polyAdd xs ys = zipWith (\x y -> (x + y) `mod` q) xs ys

-- ----------------------------------------------------
-- -- 3. 密鑰生成
-- -- 私鑰 sk 為一個隨機多項式
-- -- 公鑰 pk 為私鑰加上一個隨機雜訊多項式
-- ----------------------------------------------------
-- generateKeyPair :: IO ([Int], [Int])
-- generateKeyPair = do
--     sk    <- generatePoly         -- 生成私鑰
--     noise <- generatePoly         -- 生成雜訊
--     let pk = polyAdd sk noise     -- 公鑰 = 私鑰 + 雜訊
--     return (pk, sk)

-- ----------------------------------------------------
-- -- 4. 加密
-- -- 輸入公鑰與明文（均為多項式），生成密文：
-- -- ciphertext = 公鑰 + 明文 + 一個隨機雜訊
-- ----------------------------------------------------
-- encrypt :: [Int] -> [Int] -> IO [Int]
-- encrypt pk message = do
--     noise <- generatePoly
--     let ciphertext = polyAdd (polyAdd pk noise) message
--     return ciphertext

-- ----------------------------------------------------
-- -- 5. 解密
-- -- 利用密文減去私鑰，還原出明文
-- ----------------------------------------------------
-- decrypt :: [Int] -> [Int] -> [Int]
-- decrypt sk ciphertext = zipWith (\c s -> (c - s) `mod` q) ciphertext sk

-- ----------------------------------------------------
-- -- 6. 主程式：測試整個流程
-- ----------------------------------------------------
-- main :: IO ()
-- main = do
--     -- 生成密鑰對
--     (pk, sk) <- generateKeyPair
--     -- 生成一個明文多項式（實際中需先編碼訊息）
--     message <- generatePoly

--     -- 加密與解密
--     ciphertext <- encrypt pk message
--     let decrypted = decrypt sk ciphertext

--     -- 輸出結果
--     putStrLn "Original Message:"
--     print message
--     putStrLn "Ciphertext:"
--     print ciphertext
--     putStrLn "Decrypted Message:"
--     print decrypted

--     if message == decrypted
--       then putStrLn "Decryption Successful!"
--       else putStrLn "Decryption Failed!"
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import System.Random (randomRIO)

-- -- 模數 q 與多項式長度 n
-- q :: Int
-- q = 3329      -- Kyber 使用的素數模數
-- n :: Int
-- n = 256       -- 多項式的長度

-- ----------------------------------------------------
-- -- 1. 生成多項式
-- -- 隨機生成一個長度為 n，每個係數介於 0 到 q-1 之間的多項式
-- ----------------------------------------------------
-- generatePoly :: IO [Int]
-- generatePoly = mapM (\_ -> randomRIO (0, q - 1)) [1..n]

-- ----------------------------------------------------
-- -- 2. 多項式加法與減法（皆對 q 取模）
-- ----------------------------------------------------
-- polyAdd :: [Int] -> [Int] -> [Int]
-- polyAdd xs ys = zipWith (\x y -> (x + y) `mod` q) xs ys

-- polySub :: [Int] -> [Int] -> [Int]
-- polySub xs ys = zipWith (\x y -> (x - y) `mod` q) xs ys

-- ----------------------------------------------------
-- -- 3. 密鑰生成
-- -- 產生秘密金鑰 sk 與雜訊多項式 e，再計算公鑰 pk = sk + e
-- ----------------------------------------------------
-- generateKeyPair :: IO ([Int], [Int], [Int])
-- generateKeyPair = do
--     sk <- generatePoly    -- 隨機生成秘密金鑰
--     e  <- generatePoly    -- 隨機生成雜訊（這裡作為「誤差」）
--     let pk = polyAdd sk e  -- 公鑰 = sk + e
--     return (pk, sk, e)

-- ----------------------------------------------------
-- -- 4. 加密
-- -- 輸入：公鑰 pk、雜訊 e（我們假設 e 為公開資訊）以及明文 message
-- -- 加密時「減去 e」得到密文：
-- --   ciphertext = message − e
-- ----------------------------------------------------
-- encrypt :: [Int] -> [Int] -> [Int] -> [Int]
-- encrypt _ e message = polySub message e

-- ----------------------------------------------------
-- -- 5. 解密
-- -- 輸入：秘密金鑰 sk、公開金鑰 pk 以及密文 ciphertext
-- -- 由於 pk − sk = e，所以解密時加回 pk − sk 即可：
-- --   decrypted = ciphertext + (pk − sk) = (message − e) + e = message
-- ----------------------------------------------------
-- decrypt :: [Int] -> [Int] -> [Int] -> [Int]
-- decrypt sk pk ciphertext = polyAdd ciphertext (polySub pk sk)

-- ----------------------------------------------------
-- -- 6. 主程式：測試整個加解密流程
-- ----------------------------------------------------
-- main :: IO ()
-- main = do
--     -- 生成密鑰對：公鑰 pk、秘密金鑰 sk 與雜訊 e
--     (pk, sk, e) <- generateKeyPair

--     -- 生成一個明文多項式（實際應用中明文需先進行編碼）
--     message <- generatePoly

--     -- 加密：使用公鑰中的雜訊 e 對明文做減法
--     let ciphertext = encrypt pk e message

--     -- 解密：用 sk 和 pk 還原雜訊，再恢復明文
--     let decrypted = decrypt sk pk ciphertext

--     putStrLn "Original Message:"
--     print message
--     putStrLn "Ciphertext:"
--     print ciphertext
--     putStrLn "Decrypted Message:"
--     print decrypted

--     if message == decrypted
--       then putStrLn "Decryption Successful!"
--       else putStrLn "Decryption Failed!"
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import System.Random (randomRIO)

-- --------------------------------------------
-- -- 參數設定
-- --------------------------------------------
-- q :: Int
-- q = 3329      -- 模數，Kyber 使用的素數

-- n :: Int
-- n = 256       -- 多項式長度

-- --------------------------------------------
-- -- 1. 多項式產生
-- -- 產生長度為 n 的多項式，每個係數均採用不同分佈：
-- --   generatePoly：均勻分佈，範圍 0 ~ q-1（用於公參數 A）
-- --   generateSmallPoly：小範圍分佈，範圍 -2 ~ 2（用於密鑰、噪聲、訊息）
-- --------------------------------------------
-- generatePoly :: IO [Int]
-- generatePoly = mapM (\_ -> randomRIO (0, q - 1)) [1..n]

-- generateSmallPoly :: IO [Int]
-- generateSmallPoly = mapM (\_ -> randomRIO (-2, 2)) [1..n]

-- --------------------------------------------
-- -- 2. 多項式運算（對 q 取模）
-- --------------------------------------------
-- -- 多項式逐項加法
-- polyAdd :: [Int] -> [Int] -> [Int]
-- polyAdd xs ys = zipWith (\x y -> (x + y) `mod` q) xs ys

-- -- 多項式逐項減法
-- polySub :: [Int] -> [Int] -> [Int]
-- polySub xs ys = zipWith (\x y -> (x - y) `mod` q) xs ys

-- -- 多項式乘法：採用循環卷積（模 X^n - 1）
-- polyMul :: [Int] -> [Int] -> [Int]
-- polyMul as bs =
--   [ sum [ (as !! i) * (bs !! ((k - i) `mod` n)) | i <- [0 .. n - 1] ] `mod` q
--   | k <- [0 .. n - 1] ]

-- --------------------------------------------
-- -- 3. 公參數 A
-- -- A 為公開參數，從均勻分佈生成
-- --------------------------------------------
-- getA :: IO [Int]
-- getA = generatePoly

-- --------------------------------------------
-- -- 4. 密鑰生成
-- -- sk：秘密金鑰（小多項式）
-- -- e：噪聲（小多項式）
-- -- 公鑰 pk = A * sk + e
-- --------------------------------------------
-- generateKeyPair :: [Int] -> IO ([Int], [Int])
-- generateKeyPair a = do
--     sk <- generateSmallPoly
--     e  <- generateSmallPoly
--     let pk = polyAdd (polyMul a sk) e
--     return (pk, sk)

-- --------------------------------------------
-- -- 5. 加密
-- -- 輸入：
-- --   a: 公參數
-- --   pk: 公鑰
-- --   message: 明文（小多項式）
-- -- 加密過程：
-- --   隨機生成 r、e1、e2（皆為小多項式）
-- --   u = A * r + e1
-- --   v = pk * r + e2 + message
-- -- 輸出密文 (u, v)
-- --------------------------------------------
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import System.Random (randomRIO)

-- --------------------------------------------
-- -- 參數設定
-- --------------------------------------------
-- q :: Int
-- q = 3329       -- 模數（Kyber 使用的素數）

-- n :: Int
-- n = 256        -- 多項式長度

-- -- 我們選用 m 表示訊息 1 的編碼（訊息 0 編碼為 0）
-- -- 這裡 m 取 (q+1)//2 的中心化表示：1665 - 3329 = -1664
-- m :: Int
-- m = -1664

-- --------------------------------------------
-- -- 工具函數：將數字以 q 為模並轉為中心化區間 (-q/2, q/2]
-- --------------------------------------------
-- center :: Int -> Int
-- center x = let r = x `mod` q
--            in if r > q `div` 2 then r - q else r

-- --------------------------------------------
-- -- 1. 多項式生成
-- --------------------------------------------
-- -- 生成均勻分佈的多項式（用於公參數 A）
-- generateUniformPoly :: IO [Int]
-- generateUniformPoly = mapM (\_ -> randomRIO (0, q - 1)) [1..n]

-- -- 生成小範圍多項式（用於 sk、噪聲、隨機 r 等）
-- -- 這裡取值範圍為 {-1, 0, 1}
-- generateSmallPoly :: IO [Int]
-- generateSmallPoly = mapM (\_ -> randomRIO (-1, 1)) [1..n]

-- --------------------------------------------
-- -- 2. 多項式運算（均以模 q 運算）
-- --------------------------------------------
-- polyAdd :: [Int] -> [Int] -> [Int]
-- polyAdd xs ys = zipWith (\x y -> (x + y) `mod` q) xs ys

-- polySub :: [Int] -> [Int] -> [Int]
-- polySub xs ys = zipWith (\x y -> (x - y) `mod` q) xs ys

-- -- 多項式乘法：採用循環卷積（環 Z_q[X]/(X^n-1)）
-- polyMul :: [Int] -> [Int] -> [Int]
-- polyMul as bs =
--   [ sum [ (as !! i) * (bs !! ((k - i) `mod` n)) | i <- [0..n-1] ] `mod` q
--   | k <- [0..n-1] ]

-- --------------------------------------------
-- -- 3. 密鑰生成
-- -- 輸入公參數 A（均勻分佈多項式）
-- -- 產生秘密金鑰 sk 與噪聲 e（皆來自小範圍）
-- -- 公鑰 pk = A * sk + e
-- --------------------------------------------
-- generateKeyPair :: [Int] -> IO ([Int], [Int])
-- generateKeyPair a = do
--     sk <- generateSmallPoly
--     e  <- generateSmallPoly
--     let pk = polyAdd (polyMul a sk) e
--     return (pk, sk)

-- --------------------------------------------
-- -- 4. 訊息編碼與解碼
-- --------------------------------------------
-- -- 將二進位訊息（0 或 1）轉換為多項式表示：
-- -- 0 -> 0, 1 -> m（此處 m = -1664，代表編碼後的訊息 1）
-- encodeMessage :: [Int] -> [Int]
-- encodeMessage bits = map (\b -> if b == 0 then 0 else m) bits

-- -- 解碼：對每個係數，先以 center 轉換到 (-q/2, q/2]，
-- -- 若該係數更接近 0 則判定為 0，若更接近 m（即 -1664）則判定為 1
-- decodeCoefficient :: Int -> Int
-- decodeCoefficient x =
--   let cx = center x
--   in if abs cx < abs (cx - m) then 0 else 1

-- decodeMessage :: [Int] -> [Int]
-- decodeMessage poly = map decodeCoefficient poly

-- --------------------------------------------
-- -- 5. 加密
-- -- 輸入：
-- --   a: 公參數
-- --   pk: 公鑰
-- --   message: 編碼後的明文多項式（係數為 0 或 m）
-- -- 加密流程：
-- --   隨機生成 r, e1, e2（小多項式）
-- --   u = A * r + e1
-- --   v = pk * r + e2 + message
-- -- 輸出密文 (u, v)
-- --------------------------------------------
-- encrypt :: [Int] -> [Int] -> [Int] -> IO ([Int], [Int])
-- encrypt a pk message = do
--     r  <- generateSmallPoly
--     e1 <- generateSmallPoly
--     e2 <- generateSmallPoly
--     let u = polyAdd (polyMul a r) e1
--         v = polyAdd (polyAdd (polyMul pk r) e2) message
--     return (u, v)

-- --------------------------------------------
-- -- 6. 解密
-- -- 輸入：
-- --   sk: 秘密金鑰
-- --   (u, v): 密文
-- -- 解密流程：
-- --   w = v - (u * sk)
-- --   將 w 的每個係數解碼為 0 或 1
-- --------------------------------------------
-- decrypt :: [Int] -> ([Int], [Int]) -> [Int]
-- decrypt sk (u, v) = decodeMessage (polySub v (polyMul u sk))

-- --------------------------------------------
-- -- 7. 主程式：測試加解密流程
-- --------------------------------------------
-- main :: IO ()
-- main = do
--     -- 產生公參數 A
--     a <- generateUniformPoly
--     putStrLn "Public parameter A:"
--     print a

--     -- 生成密鑰對
--     (pk, sk) <- generateKeyPair a
--     putStrLn "Public key pk:"
--     print pk
--     putStrLn "Secret key sk:"
--     print sk

--     -- 生成隨機二進位訊息（長度 n，每個位元 0 或 1）
--     messageBits <- mapM (\_ -> randomRIO (0, 1)) [1..n]
--     putStrLn "Original message bits:"
--     print messageBits

--     -- 將訊息編碼成多項式（0 或 m）
--     let encodedMsg = encodeMessage messageBits
--     putStrLn "Encoded message poly:"
--     print encodedMsg

--     -- 加密
--     ciphertext <- encrypt a pk encodedMsg
--     putStrLn "Ciphertext (u, v):"
--     print ciphertext

--     -- 解密
--     let decryptedBits = decrypt sk ciphertext
--     putStrLn "Decrypted message bits:"
--     print decryptedBits

--     if messageBits == decryptedBits
--       then putStrLn "Decryption Successful!"
--       else putStrLn "Decryption Failed!"
-- {-# LANGUAGE OverloadedStrings #-}

-- import Crypto.Cipher.AES (AES256)
-- import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, ctrCombine, makeIV)
-- import Crypto.Error (CryptoFailable(..))
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BS8

-- -- | 使用 AES-256 CTR 模式加密
-- aes256EncryptCTR :: BS.ByteString  -- ^ 金鑰 (必須是 32 bytes)
--                  -> BS.ByteString  -- ^ IV (必須是 16 bytes)
--                  -> BS.ByteString  -- ^ 明文
--                  -> BS.ByteString  -- ^ 密文
-- aes256EncryptCTR key iv plaintext =
--   case cipherInit key :: CryptoFailable AES256 of
--     CryptoFailed err -> error ("金鑰初始化失敗: " ++ show err)
--     CryptoPassed cipher ->
--       case makeIV iv :: Maybe (IV AES256) of
--         Nothing -> error "無效的 IV"
--         Just realIV -> ctrCombine cipher realIV plaintext

-- -- | 在 CTR 模式下，加密與解密過程相同
-- aes256DecryptCTR :: BS.ByteString  -- ^ 金鑰
--                  -> BS.ByteString  -- ^ IV
--                  -> BS.ByteString  -- ^ 密文
--                  -> BS.ByteString  -- ^ 明文
-- aes256DecryptCTR = aes256EncryptCTR

-- main :: IO ()
-- main = do
--   -- 256-bit 金鑰 (32 bytes)
--   let key = "01234567890123456789012345678901"  -- 注意：實際使用中請使用隨機且保密的金鑰
--   -- 128-bit IV (16 bytes)，生產環境請使用隨機 IV
--   let iv  = "0123456789012345"
--   let plaintext = "Hello, AES256 in Haskell!"
--   let ciphertext = aes256EncryptCTR (BS8.pack key) (BS8.pack iv) (BS8.pack plaintext)
--   putStrLn $ "加密後的結果: " ++ show ciphertext
--   let decrypted = aes256DecryptCTR (BS8.pack key) (BS8.pack iv) ciphertext
--   putStrLn $ "解密後的結果: " ++ BS8.unpack decrypted

-- {-# LANGUAGE OverloadedStrings #-}

-- import Crypto.Cipher.AES (AES256)
-- import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, ctrCombine, makeIV)
-- import Crypto.Error (CryptoFailable(..))
-- import qualified Data.ByteString.Char8 as BS8
-- import Data.ByteArray (convert)
-- import Data.ByteArray (ScrubbedBytes)

-- -- | 使用 AES-256 CTR 模式進行加密
-- aes256EncryptCTR :: BS8.ByteString  -- 金鑰 (32 bytes)
--                  -> BS8.ByteString  -- IV (16 bytes)
--                  -> BS8.ByteString  -- 明文
--                  -> BS8.ByteString  -- 密文
-- aes256EncryptCTR key iv plaintext =
--   case cipherInit (convert key :: ScrubbedBytes) :: CryptoFailable AES256 of
--     CryptoFailed err -> error ("金鑰初始化失敗: " ++ show err)
--     CryptoPassed cipher ->
--       case makeIV iv :: Maybe (IV AES256) of
--         Nothing -> error "無效的 IV"
--         Just realIV -> ctrCombine cipher realIV plaintext

-- -- | CTR 模式下加密與解密相同
-- aes256DecryptCTR :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> BS8.ByteString
-- aes256DecryptCTR = aes256EncryptCTR

-- main :: IO ()
-- main = do
--   let key       = "01234567890123456789012345678901"  -- 32 bytes 的金鑰
--       iv        = "0123456789012345"                  -- 16 bytes 的 IV
--       plaintext = "Hello, AES256 in Haskell!"
--       ciphertext = aes256EncryptCTR key iv plaintext
--   putStrLn $ "加密後的結果: " ++ show ciphertext
--   let decrypted = aes256DecryptCTR key iv ciphertext
--   putStrLn $ "解密後的結果: " ++ BS8.unpack decrypted
-- {-# LANGUAGE OverloadedStrings #-}

-- import Crypto.Cipher.AES (AES256)
-- import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, ctrCombine, makeIV)
-- import Crypto.Error (CryptoFailable(..))
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as C

-- -- | 使用 AES-256 CTR 模式加密 (與解密相同)
-- aes256EncryptCTR :: B.ByteString -- ^ 金鑰 (32 bytes)
--                  -> B.ByteString -- ^ IV (16 bytes)
--                  -> B.ByteString -- ^ 明文
--                  -> B.ByteString -- ^ 密文
-- aes256EncryptCTR key iv plaintext =
--   case cipherInit key :: CryptoFailable AES256 of
--     CryptoFailed err -> error ("金鑰初始化失敗: " ++ show err)
--     CryptoPassed cipher ->
--       case makeIV iv :: Maybe (IV AES256) of
--         Nothing     -> error "無效的 IV"
--         Just realIV -> ctrCombine cipher realIV plaintext

-- -- | CTR 模式下加密與解密過程相同
-- aes256DecryptCTR :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
-- aes256DecryptCTR = aes256EncryptCTR

-- main :: IO ()
-- main = do
--   -- 明確標註型別為 Data.ByteString (來自 Data.ByteString)
--   let key :: B.ByteString
--       key = "01234567890123456789012345678901"  -- 32 bytes 的金鑰

--       iv :: B.ByteString
--       iv = "0123456789012345"                  -- 16 bytes 的 IV

--       plaintext :: B.ByteString
--       plaintext = "Hello, AES256 in Haskell!"

--       ciphertext = aes256EncryptCTR key iv plaintext

--   putStrLn $ "加密後的結果: " ++ show ciphertext
--   let decrypted = aes256DecryptCTR key iv ciphertext
--   putStrLn $ "解密後的結果: " ++ C.unpack decrypted

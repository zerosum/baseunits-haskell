module Money
  ( Money (),
    Amount,
    money,
    breachEncapsulationOfAmount,
    breachEncapsulationOfCurrency,
    adjustBy,
    adjustByDouble,
    plus,
    times,
  )
where

import Currency (Alpha (..), Currency (..), fromAlpha)
import Data.Fixed

-- | 金額を表すデータ型.
--
--   「量」と「通貨単位」からなる
data Money = Money
  { -- | 通貨量
    amount :: Amount,
    -- | 通貨単位
    currency :: Currency
  }
  deriving (Show)

-- | Amount型の値と通貨のアルファベットコードから, 金額を導出する.
--
--   minor unitを超えた桁の通貨量が入力された場合は丸めは行わず, 未定義として失敗する
money :: Amount -> Alpha -> Money
money a alpha
  | isValidScale = Money a (fromAlpha alpha)
  | otherwise = undefined
  where
    Currency _ minor = fromAlpha alpha
    x = a * (10 ^ 6)
    y = 10 ^ (6 - minor)
    isValidScale = mod' x y == 0

breachEncapsulationOfAmount :: Money -> Amount
breachEncapsulationOfAmount (Money a _) = a

breachEncapsulationOfCurrency :: Money -> Currency
breachEncapsulationOfCurrency (Money _ c) = c

-- | 通貨量を任意精度小数で表すための型シノニム. 精度は小数点以下6桁(μ)で十分とする.
type Amount = Fixed E6

-- | 「金額」は等値性を持つため, Eq型クラスのインスタンスとして扱う.
--
--   通貨単位が異なる場合でも, 量が`0`である金額同士は等値であるとみなす.
--
--   例えば`0 USD`と`0 JPY`は等しい.
instance Eq Money where
  (Money 0 _) == (Money 0 _) = True
  (Money a c) == (Money a' c') = a == a' && c == c'

-- | 「金額」は大小比較を行うことができるため, Ord型クラスのインスタンスとして扱う.
--
--   比較対象の通貨単位が異なる場合でも, いずれか一方の量が`0`である金額同士は量の値のみで大小比較を行う.
--   そうでない場合の比較は未定義とし, 失敗する.
instance Ord Money where
  compare (Money 0 _) (Money a' _) = compare 0 a'
  compare (Money a _) (Money 0 _) = compare a 0
  compare (Money a c) (Money a' c')
    | c == c' = compare a a'
    | otherwise = undefined

-- | Amount型の値と通貨のアルファベットコードから, 金額を導出する.
--
--   通貨量の丸め（四捨五入）を行う.
adjustBy :: Amount -> Alpha -> Money
adjustBy a alpha = money (round' a cur) alpha
  where
    cur = Currency.fromAlpha alpha

-- | Double型の値と通貨のアルファベットコードから, 金額を導出する.
--
--   通貨量は通貨単位に定義されている分量単位(minor unit)に応じた丸め（四捨五入）を行う.
adjustByDouble :: Double -> Alpha -> Money
adjustByDouble d a = money (round' (realToFrac d) cur) a
  where
    cur = Currency.fromAlpha a

-- | 通貨量を, 通貨単位に応じた分量単位の桁に丸める（四捨五入）.
round' :: Amount -> Currency -> Amount
round' a (Currency _ m) = fromInteger (round $ a * (10 ^ m)) / (10.0 ^^ m)

-- | 金額同士の加算.
--
--   異なる通貨単位同士の加算は未定義とし, 失敗する.
plus :: Money -> Money -> Money
plus m@(Money 0 _) _ = m
plus _ m'@(Money 0 _) = m'
plus (Money a c@(Currency alpha _)) (Money a' c')
  | c /= c' = undefined
  | otherwise = adjustBy (a + a') alpha

-- | 金額にDouble型の値を掛けた金額を導出する.
--
--   計算結果は四捨五入で丸める.
times :: Money -> Double -> Money
times (Money a (Currency alpha _)) d = adjustBy (a * realToFrac d) alpha

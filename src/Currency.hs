module Currency(
    Currency(..)
    , Alpha(..)
    , fromAlpha
) where

-- | 通貨単位を表す型. ISO4217のtiny setである.
data Currency = Currency
    { alpha :: Alpha -- ^ 通貨のアルファベットコード
    , minor :: Int   -- ^ 通貨の分量単位（minor units）
    } deriving (Show, Eq)

data Alpha
     = EUR -- ^ Euro
     | JPY -- ^ Japanese Yen
     | USD -- ^ US Dollar
     deriving (Show, Eq)

fromAlpha :: Alpha -> Currency
fromAlpha alpha =
    case alpha of
        EUR -> Currency EUR 2
        JPY -> Currency JPY 0
        USD -> Currency USD 2

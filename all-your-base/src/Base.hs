module Base
  ( Error(..)
  , rebase
  , toDecimal
  , fromDecimal
  )
where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

toDecimal :: Integral a => a -> [a] -> Either (Error a) a
toDecimal inputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | otherwise     = foldl reduce (Right 0) $ (zip . reverse) inputDigits [0 ..]
 where
  reduce acc (i, y) | i >= inputBase || i < 0 = Left $ InvalidDigit i
                    | otherwise = (+) <$> acc <*> (Right $ i * inputBase ^ y)



fromDecimal :: Integral a => a -> a -> Either (Error a) [a]
fromDecimal outputBase input
  | outputBase < 2
  = Left InvalidOutputBase
  | input == 0
  = Right []
  | otherwise
  = let division = input `div` outputBase
        rest     = input `rem` outputBase
        result   = Right [rest]
    in  if division < 1
          then result
          else (++) <$> fromDecimal outputBase division <*> result



rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits =
  toDecimal inputBase inputDigits >>= fromDecimal outputBase

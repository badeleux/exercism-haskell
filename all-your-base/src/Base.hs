module Base (Error(..), rebase, toDecimal) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

toDecimal :: Integral a => a -> [a] -> Either (Error a) a
toDecimal inputBase inputDigits | inputBase < 2 = Left InvalidInputBase 
                                | otherwise = Right $ foldl reduce 0 $ (zip . reverse) inputDigits [0..] 
                                     where
                                      reduce acc (i,y) = acc + y * inputBase^i

fromDecimal :: Integral a => a -> a -> Either (Error a) a
fromDecimal ouputBase input | outputBase < 2 = Left InvalidOuputBase 
  | otherwise = let division = input / outputBase 
                    rest = input `rem` outputBase 
                    result = Right [rest]
                 in if division < 1 
                       then result
                       else result >>= 



rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits | outputBase < 2 = Left InfvalidOutputBase
  | Right 

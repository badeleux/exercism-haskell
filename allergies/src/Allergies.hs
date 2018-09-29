module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Enum)

toCode :: Allergen -> Int
toCode Eggs = 1
toCode Peanuts = 2
toCode Shellfish = 4
toCode Strawberries = 8
toCode Tomatoes = 16
toCode Chocolate = 32 
toCode Pollen = 64
toCode Cats = 128


list = enumFrom Eggs

allergies :: Int -> [Allergen]
allergies score = filter (flip isAllergicTo score) (enumFrom Eggs)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = toCode allergen .&. score > 0

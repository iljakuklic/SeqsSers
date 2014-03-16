
{-# LANGUAGE NoMonomorphismRestriction #-}

module TenAdic where

import Data.Ord


-- -------------------   DECIMAL DIGITS   ---------------------

-- | Decimal digits
data Dec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
           deriving (Eq, Ord, Enum, Show)

-- | Perform a binary arithmetic operation on digits.
-- Returns the carry digit and the least significant digit.
-- Assumes the result <= 99
liftD2 :: (Int -> Int -> Int) -> (Dec -> Dec -> (Dec, Dec))
liftD2 op x y = (toEnum (r `div` 10), toEnum (r `mod` 10))
  where r = fromEnum x `op` fromEnum y

-- | addition & multiplication of digits
addD, multD :: Dec -> Dec -> (Dec, Dec)
addD  = liftD2 (+)
multD = liftD2 (*)

-- | 'negate' a decimal digit (e.g. 9 -> 0, 7 -> 2, 3 -> 6, etc.)
negD :: Dec -> Dec
negD = toEnum . (9-) . fromEnum


-- --------------------   10-ADIC NUMBERS   ---------------------

-- | 10-adic numbers
data TenAdic = TenAdic :~ Dec | Zeros | Nines
infixl 5 :~

-- display 10-adic numbers
instance Show TenAdic where
    show Zeros = "...0000"
    show Nines = "...9999"
    show (ds :~ d) = show ds ++ (tail $ show d)

-- | Convert 10-adic number to an Integer.
asInteger :: TenAdic -> Integer
asInteger Zeros = 0
asInteger Nines = -1
asInteger (ds :~ d) = asInteger ds * 10 + toInteger (fromEnum d)

-- Compare 10-adic numbers.
instance Ord TenAdic where
    compare = comparing asInteger
instance Eq TenAdic where
    x == y = (x `compare` y) == EQ

-- Listing 10-adic numbers
instance Enum TenAdic where
    toEnum   = fromInteger . toInteger
    fromEnum = fromInteger . asInteger

-- | Create a 10-adic number from a digit
tad :: Dec -> TenAdic
tad D0 = Zeros
tad d  = Zeros :~ d

-- | Multiply a 10-adic number by a digit.
(*.) :: TenAdic -> Dec -> TenAdic
Zeros     *. _  = Zeros
_         *. D0 = Zeros
x         *. D1 = x
Nines     *. d  = (Nines :~ negD d) + 1
(xs :~ x) *. y  = let (c, r) = x `multD` y in xs *. y + tad c :~ r
infixl 7  *.

-- | Arithmetic on 10-adic numbers
instance Num TenAdic where

    -- addition
    Zeros + y = y
    x + Zeros = x
    Nines + Nines = Nines :~ D8
    Nines + (Zeros :~ D1) = Zeros
    Nines + y = (Nines :~ D9) + y
    x + Nines = Nines + x
    (xs :~ x) + (ys :~ y) = let (c, r) = x `addD` y
                            in xs + ys + tad c :~ r

    -- multiplication
    Zeros * y = Zeros
    x * Zeros = Zeros
    Nines * y = negate y
    x * Nines = negate x
    -- Distribute the 1st parens: (xs + x) * (ys + y) = (xs + x) * ys + xs * y + x * y
    (xs :~ x) * (ys :~ y) = let (c, r) = x `multD` y
                            in (xs :~ x) * ys + xs *. y + tad c :~ r

    -- negation
    negate Nines = 1
    negate Zeros = Zeros
    negate (Zeros :~ D1) = Nines
    negate x = neg' x + 1
      where
        neg' Zeros = Nines
        neg' Nines = Zeros
        neg' (ds :~ d) = neg' ds :~ negD d

    -- nice numeric literals
    fromInteger x | x < 0 = negate (fromInteger (-x))
    fromInteger 0 = Zeros
    fromInteger x = let (x10, x1) = x `divMod` 10 in fromInteger x10 :~ toEnum (fromInteger x1)

    -- signum & abs appear to be tedious to write... delegate to Integers
    signum = fromInteger . signum . asInteger
    abs = fromInteger . abs . asInteger

{- MyInt module of exercise 4.
 - author: Luis Henrique Paoletti
 - date: 11.11.24
 -}
module MyInt where
import Digit

data MyInt = F Digit          -- as in 'final digit'
             | D Digit MyInt  -- as in 'digit'


instance Show MyInt where
    show (F d)    = show d
    show (D d mi) = show d ++ show mi

instance Read MyInt where
    readsPrec _ input =
        -- strip leading whitespace out
        let input' = dropWhile (== ' ') input
        in case reads input' of
            -- if last digit, then end recursion
            [(d :: Digit, ""  )] -> [(F d, "")]
            [(d :: Digit, rest)] ->
                case reads rest of
                    -- return something only if the rest is MyInt, which implicitly recurses on `rest`
                    [(mi :: MyInt, rest')] -> [(D d mi, rest')]
                    -- return nothing if `rest` not MyInt
                    _ -> []
            -- return nothing if not Digit
            _ -> []

instance Eq MyInt where
    (==) (F d1)     (F d2)     = d1 == d2
    (==) (D d1 mi1) (D d2 mi2) = d1 == d2 && mi1 == mi2
    -- always False when different lengths
    (==) _          _          = False

-- instance Ord MyInt where
--     (<=) (F d1) (F d2) = d1 <= d2
--     (<=) mi1 mi2 =
--         -- pad to same length to compare bitwise
--         let ((D d1 mi1), (D d2 mi2)) = pad mi1 mi2
--         in if d1 == d2
--            then mi1 <= mi2
--            else d1 < d2
-- TODO this is actually wrong; base -2 works differently than base 2


-- Utilities --

{- Translate an Integer to a MyInt. -}
-- myIntFromInteger :: Integer -> MyInt
-- myIntFromInteger i = 
-- TODO

{- Implement the integer division **correctly**.
 - Haskell's `div` implementation is incorrect because
 - 7 `div` (-2) = -4 , with rest -1
 - but actually
 - 7 / (-2) = -3 , with rest 1
 - is the correct integer division, because
 - -3 * -2 + 1 = 7 . -}
myDivMod :: Integer -> Integer -> (Integer, Integer)
myDivMod = findQuoRem 0

{- Find the quotient and remainder of an integer division.
 - To get the quotient, an accumulation is used, which should begin with 0 (up to the calling code).
 - Each accumulator step is the next multiplier of `divisor`. -}
findQuoRem :: Integer -> Integer -> Integer -> (Integer, Integer)
findQuoRem remainder dividend divisor =
    if abs dividend < abs divisor
    then (remainder, dividend)
    else if signum dividend == signum divisor
         then findQuoRem (remainder + 1) sameSignDividend     divisor
         else findQuoRem (remainder - 1) (dividend + divisor) divisor

    where sameSignDividend = dividend + oppositeSign * abs divisor
          oppositeSign = negate $ signum divisor

{- Digit module of exercise 4.
 - author: Luis Henrique Paoletti
 - date: 11.11.24
 -}
module Digit where

data Digit = Zero | One

instance Show Digit where
    show Zero = "0"
    show One  = "1"

instance Eq Digit where
    (==) Zero Zero = True
    (==) One  One  = True
    (==) _    _    = False

instance Ord Digit where
    (<=) One  Zero = False
    (<=) _    _    = True

instance Enum Digit where
    toEnum 1 = One
    toEnum _ = Zero
    fromEnum Zero = 0
    fromEnum One  = 1

instance Read Digit where
    readsPrec _ input =
        let input' = dropWhile (== ' ') input
        in case input' of
            ('0':rest) -> [(Zero, rest)]
            ('1':rest) -> [(One , rest)]
            _          -> []

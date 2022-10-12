-- A reminder to myself of type behaviour in Haskell
-- Date of Creation: 12/10/2022
-- Date of Last Edit: 12/10/2022
-- Author: Kyle Dick, kd41@hw.ac.uk

{-

    Haskell uses type inference.
    Types can be declared however by using ::

    Prelude> 5 :: Int
    5
    Prelude> 5 :: Double
    5.0

    Types and Classes always start with upper-case letters. 
    Variables always start with lower-case.  
    Rule of language, not naming convention.  

    The ghci compiler can also type check.  

    Prelude> :t True
    True :: Bool
    Prelude> :t 'X'
    'X' :: Char
    Prelude> :t "Hello, Haskell"
    "Hello, Haskell" :: [Char]      --- [Char] == String

    The following types use 'type classes'
     - 42 can be used as any numeric type
     - 42.0 can be any fractional type (but not an integral type)
     - gcd 15 20 can be any integral type but not a fractional

    Prelude> :t 42
    42 :: (Num t) => t
    Prelude> :t 42.0 
    42.0 :: (Fractional t) => t
    Prelude> :t gcd 15 20
    gcd 15 20 :: (Integral t) => t 

    Five numeric types in Haskell prelude library.
    All five are instances of Num type class.
        - Int, 30 bits of precision (Integral)
        - Integer, unlimited precision (Integral)
        - Float, single precision floating point (Fractional)
        - Double, double precision floating point (Fractional)
        - Rational, fraction type, no rounding error (Fractional)

    () - unit type, only one value written as ().
    This behaves similar to void in C

    Prelude> ()
    ()
    Prelude> :t ()
    () :: ()
-}
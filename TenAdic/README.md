10-adic numbers
===============

The [TenAdic.hs](TenAdic.hs) file contains an experimental implementation of 10-adic numbers
including basic arithmetic (addition, multiplication, negation). Haskell provides subtraction
and exponentiation by a natural number for free.

It is best to try it out by firing up Haskell interpreter:

```
$ ghci TenAdic.hs
```

The infinite sequence of nines is represented by the `Nines` constructor,
similarly with `Zeros`. A digit can be appended using the `:~` operator.
Decimal digits are represented by constructors `D0` to `D9`.

A few examples:

```Haskell
λ> Zeros :~ D3         --  (3)
...00003

λ> Nines               -- (-1)
...9999

λ> Nines :~ D5         -- (-5)
...99995

λ> Zeros :~ D1 :~ D4   -- (14)
...000014
```

Let's try some arithmetic (`:~` has a low operator precedence, parenthesise carefully):

```Haskell
λ> (Zeros :~ D3) - (Zeros :~ D7)
...99996

λ> Nines + (Zeros :~ D1)
...0000

λ> Nines * Nines
...00001
```

We can convert 10-adic number to a regular integer:

```Haskell
λ> (Nines :~ D7)
...99997

λ> asInteger (Nines :~ D7)
-3
```

Numeric literals work provided we give the interpreter type hint we want a `TenAdic` number:

```Haskell
λ> -762241 :: TenAdic
...9999237759

λ> [ x^3 | x <- [-4..2] ] :: [TenAdic]
[...9999976,...9999903,...9999912,...9999,...0000,...00001,...00008]
```

That's it for now, have fun.

# haskell-lvalue: Bring C-style lvalues to haskell!

## Rationale

Recently I stumbled across [a](http://augustss.blogspot.com/2007/08/programming-in-c-ummm-haskell-heres.html) [nice](http://augustss.blogspot.com/2007/08/what-about-arrays-after-doing-my-little.html) [overview](https://web.archive.org/web/20070823214707/http://augustss.blogspot.com/2007/08/quicksort-in-haskell-quicksort-is.html) on how we can embed a C-style programming language with lvalues in haskell, and wondered *why did no-one write a library for that?*. And here I am.

Mind, this library is at the current stage a bit more than a hack. While it *tries* to make working with `IORefs` (the Haskell equivalent of imperative languages stateful variables) a lot easier, it should not be used as a replacement for functional programming.

This library attempts to implement with the means of [General Algebraic Data Types](https://wiki.haskell.org/GADTs_for_dummies) a type-safe version of lvalues. Your code won't compile if you try to use them wrong [¹](#note1).

## Usage

To make it C-like, the module exports lvalues and rvalues on basic data types (basically wrapping IORefs) and a nice C-like syntax for 1-D arrays.

Some operators have been defined, but they are meant to be used like the C ones (e.g. `>, >=, ||, &&` etc.) and thus can clash the prelude. Please import them qualified.

That said, let's look at how to use it:

### new, copy, assignment (`=:`) and constant

```haskell
{-# LANGUAGE NoImplicitPrelude#-}

import Control.LValue

main = do
    x <- new 3
    y <- copy x
    y =: x * y
    runExpression y >>= \val -> putStrLn "Now y contains" ++ show val

    -- (x + y) =: new 42 -- Error: rvalues are not assignable
    let z = constant 42
    x += z
    y += z
    -- z += z -- Error: constants are rvalues, thus not assignable
    runExpression y >>= \val -> putStrLn "Now y contains" ++ show val
```

Expected output:

```
Now y contains 9
Now y contains 51
```

Internally, `x`, `y`, `z` are instances of `Expr' v a`. `v` is a [phantom type](https://wiki.haskell.org/Phantom_type) used to make conversions between lvalues and rvalues possible in only one way.

The function `runExpression` takes an expression in the form `Expr' v a` and returns an `IO a` containing the value.

How do `new`, `copy` and `constant` differ? Simple:

- `new` generates a **new lvalue** given a naked value
- `copy` copies an **existing** \*value and copies it into a **new lvalue**
- `constant` generates a **new rvalue** given a naked value. It does not create a new `IORef` internally and can be conceptually seen as a C-style `const` (i.e. not modifiable, not necessarily a constant literal).

## TODO

- [ ] Add an example for arrays
- [ ] Implement while loops
- [ ] Implement for loops
- [ ] Make some IO wrappers
- [ ] Make it work outside the IO monad (e.g. ST monad)
- [ ] Possibly integrate Template Haskell

## Notes

<a name="note1">¹</a>: At the right stage, *some* operations rely on runtime check, for example array indexing requires the lists to only have one element.

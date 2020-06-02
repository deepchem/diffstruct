# diffstruct

This repo holds some simple experiments with differentiation. For now,
I'm using this to hold a few simple haskell intepreter scripts I've
been playing with in hopes of understanding autodifferentiation and language design better.

Simple interpreters are implemented using Haskell. To install GHC (the haskell compiler), I recommend using [ghcup](https://www.haskell.org/ghcup/).

To run the code in this repo, I suggest using
[GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html)
to interactively run the code. Here's an example of how to load an
interpreter into GHCi:

```
(base) bharath@Bharaths-MBP diffstruct % ghci                         
GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
Prelude> :load minimal.hs
[1 of 1] Compiling Main             ( minimal.hs, interpreted )
Ok, one module loaded.
*Main> 
```

minimal.hs
----------
This intepreter follows this [stackoverflow answer](https://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell). It implements an intepreter for a simple imperative language.

minimal_monadic.hs
----------
This interpreter follows this [stackoverflow
answer](https://stackoverflow.com/questions/16970431/implementing-a-language-interpreter-in-haskell).
It implements an intepreter for a simple imperative language using a
simple `Interp` monad to do error handling.

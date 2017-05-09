# higher-rank

A small Haskell implementation of [Complete and Easy Bidirectional Typechecking
for Higher-Rank Polymorphism][complete-and-easy]. This is a sister project to [the Racket implementation][racket-higher-rank], which is adapted from this one.

**This branch extends the system in the paper with product and sum types**, so it diverges slightly from the paper in places. For an implementation that mirrors the paper directly, [see the master branch][higher-rank-master].

This implementation is designed to be both simple and relatively complete. Where the two conflict, it prefers simplicity. This means that there is no source location reporting for type errors, for example, which would considerably complicate the implementation, but it *does* attempt to provide good error messages with the information it has.

The executable built by this project implements a simple REPL. You can run it from the command line with `stack`:

```
$ stack build
$ stack exec higher-rank
> ()
() : ()
> (\x -> x)
(\x -> x) : (a1' -> a1')
> ((\x -> x) ())
() : ()
```

The implementation is divided among the following modules:

  - `Language.HigherRank.Types` — Holds type definitions used by other modules. This module mostly only exists to break module loading cycles between the printer and interpreter/typechecker.

  - `Language.HigherRank.Typecheck` — The core implementation of the typechecker. If you are only interested in the typechecking algorithm and aren’t interested in the interpreter or REPL, you can focus exclusively on this module and `Language.HigherRank.Types`.

  - `Language.HigherRank.Interpret` — Contains the implementation of a very simple interpreter, which evaluates expressions without doing any typechecking.

  - `Language.HigherRank.Desugar` — Contains a tiny desugarer, which desugars product and sum syntax into function calls to simplify the interpreter and typechecker.

  - `Language.HigherRank.Parse` — Contains monadic parsers for parsing both types and expressions, the results of which may be fed to the typechecker or interpreter (after desugaring).

  - `Language.HigherRank.Print` — Implements pretty-printers for types, expressions, and reduced expressions, which are the results of the interpreter. This is used to print results in the REPL as well as format types and expressions in error messages.

  - `Language.HigherRank.Util.TH` — Provides quasi-quoters for expressions and types that use the parsers in `Language.HigherRank.Parse`. This is just to make writing expressions and types shorter, used in the typechecker to write the types of builtins and for examples in the test suite.

  - `Language.HigherRank.Main` — This implements the actual REPL by combining all of the above pieces together.

[complete-and-easy]: http://www.cs.cmu.edu/~joshuad/papers/bidir/
[higher-rank-master]: https://github.com/lexi-lambda/higher-rank
[racket-higher-rank]: https://github.com/lexi-lambda/racket-higher-rank

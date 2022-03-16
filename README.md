# xic-hs

A parser for the [Xi programming language](https://www.cs.cornell.edu/courses/cs4120/2022sp/project/language.pdf)
written in Haskell using [Megaparsec](https://hackage.haskell.org/package/megaparsec).

## Building and Running

Build using `stack`.

```
cd xic-hs/
stack build
```

The `Main.hs` file takes in a programming through standard
input and produces an AST.

```
stack run < program.xi
```

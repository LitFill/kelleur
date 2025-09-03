# LitFill's Kelleur

My solutions to Project Euler puzzles in Haskell.

Use cabal to build and run:

```sh
cabal build                                # will build the exe
cabal run kelleur 1                        # print the solution to the 1st problem
echo {1..10} | xargs -n1 cabal run kelleur # print the solutions to the problem 1-10.
./test 1 10                                # use the runghc script to do the same
```

## Requirements

*   This project requires the `md5sum` command-line tool to verify the solutions.
*   It is built with `ghc` version 9.12.2.
*   The project uses the `GHC2024` language standard and requires `base` library version `^>=4.21.0.0`.

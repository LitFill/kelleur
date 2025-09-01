# LitFill's Kelleur

My solutions to Project Euler puzzles in Haskell.

Use cabal to build and run:

```sh
cabal build                                # will build the exe
cabal run kelleur 1                        # print the solution to the 1st problem
echo {1..10} | xargs -n1 cabal run kelleur # print the solutions to the problem 1-10.
```

## Requirements

*   This project requires the `md5sum` command-line tool to verify the solutions.
*   It is built with `cabal` version 3.0 or higher.
*   The project uses the `GHC2024` language standard and requires `base` library version `^>=4.21.0.0`.

# [Advent Of Code (AOC)](https://adventofcode.com)

Advent Of Code ([AOC](https://adventofcode.com)) solutions in [Haskell](https://www.haskell.org/)

## Getting Started

1. Go to each day folder
2. Execute `main.hs` file using `ghc main.hs`
3. Each day folder contains `sample.txt` and `input.txt` containing the puzzle's sample and input data respectively.
4. Exception is [2015/day4](./2015/day4/), as this requires external dependencies-
   1. Might have to install `libgmp3-dev`
   ```console
   sudo apt-get install libgmp3-dev
   ```
   2. Do `cabal build` inside the [folder](./2015/day4/)
   ```console
   cabal build
   ```
   3. Execute `cabal run` to run
   ```console
   cabal run
   ```

## Copyright

@Sinha-Ujjawal

Licensed under [MIT](./LICENSE)

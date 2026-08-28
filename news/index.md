# Changelog

## proporz 1.5.3

- feature: enable csv input/output in shiny app
  ([`run_app()`](https://polettif.github.io/proporz/reference/run_app.md))
- bugfix:
  [`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
  and
  [`upper_apportionment()`](https://polettif.github.io/proporz/reference/upper_apportionment.md):
  `weight_votes` is ignored if number of seats is a single number
- bugfix: assert vector-matrix name matching in
  [`lower_apportionment()`](https://polettif.github.io/proporz/reference/lower_apportionment.md)
- improved error handling and messages for input parameters

## proporz 1.5.2

CRAN release: 2025-10-10

- weight_list_votes() is deprecated, renamed to
  [`weight_votes_matrix()`](https://polettif.github.io/proporz/reference/weight_votes_matrix.md)
- Parameter `use_list_votes` in
  [`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)/[`pukelsheim()`](https://polettif.github.io/proporz/reference/pukelsheim.md)
  is deprecated, renamed to `weight_votes`
- [`proporz::proporz()`](https://polettif.github.io/proporz/reference/proporz.md)
  can now be used without calling
  [`library(proporz)`](https://polettif.github.io/proporz/)
- [`pukelsheim()`](https://polettif.github.io/proporz/reference/pukelsheim.md)
  returns tibbles for tibble input

## proporz 1.5.1

CRAN release: 2025-03-10

- new “Modifying biproporz()” vignette
  ([\#15](https://github.com/polettif/proporz/issues/15))
- add
  [`district_winner_matrix()`](https://polettif.github.io/proporz/reference/district_winner_matrix.md)
  function, exported internal implementation
  ([\#13](https://github.com/polettif/proporz/issues/13))
- add
  [`apply_quorum()`](https://polettif.github.io/proporz/reference/apply_quorum.md)
  function, exported internal implementation
- add [`summary()`](https://rdrr.io/r/base/summary.html) method for
  biproporz results which shows marginal sums and divisors
- returned seat values from proporz/biproporz functions are always
  integer
- undefined biproporz results are caught earlier, added flow criterion
  check ([\#14](https://github.com/polettif/proporz/issues/14))
- minor changes to error messages

## proporz 1.5.0

CRAN release: 2024-04-03

- add new feature: winner take one method for
  [`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
  and
  [`pukelsheim()`](https://polettif.github.io/proporz/reference/pukelsheim.md)
  ([\#10](https://github.com/polettif/proporz/issues/10))
- allow non-integer vote counts
- round divisors to as few digits as possible
- changed error message for undefined/tied results
- fixed bugs and added more real data tests

## proporz 1.4.0

CRAN release: 2024-03-04

- first CRAN release
- add vignette and examples
- removed `biproportional()`, only use
  [`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
- renamed `quota_largest_remainder()` to
  [`largest_remainder_method()`](https://polettif.github.io/proporz/reference/largest_remainder_method.md)
- renamed `divisors()` to
  [`get_divisors()`](https://polettif.github.io/proporz/reference/get_divisors.md)
- removed `hzv()` alias and made
  [`reached_quorums()`](https://polettif.github.io/proporz/reference/reached_quorums.md)
  internal
- update documentation and tests
- minor bugfixes

## proporz 1.3.1

- error if not enough seats are available for harmonic/geometric divisor
  methods
- always error on equal remainders

## proporz 1.3.0

- rework and expand quorum calculations
  ([\#4](https://github.com/polettif/proporz/issues/4))
- deprecated quorum_total and quorum_districts parameters

## proporz 1.2.1

- allow number of seats to be 0 instead of throwing error

## proporz 1.2.0

- throw error if no solution can be found
- added basic shiny app

## proporz 1.1.0

- apportion method and use_list_votes parameter for biproportional()
- add finland2019 dataset
- export lower and upper apportionment functions
- improve divisor search

## proporz 1.0.0

- Publish package to Github

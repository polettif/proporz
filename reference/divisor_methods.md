# Divisor methods

Functions to directly apply divisor apportionment methods instead of
calling
[`proporz()`](https://polettif.github.io/proporz/reference/proporz.md)
with a method parameter. All divisor functions call
[`highest_averages_method()`](https://polettif.github.io/proporz/reference/highest_averages_method.md)
with a different sequence of divisors.

## Usage

``` r
divisor_round(votes, n_seats, quorum = 0)

divisor_floor(votes, n_seats, quorum = 0)

divisor_harmonic(votes, n_seats, quorum = 0)

divisor_geometric(votes, n_seats, quorum = 0)

divisor_ceiling(votes, n_seats, quorum = 0)
```

## Arguments

- votes:

  Numeric vector with the number of votes for each party

- n_seats:

  Total number of seats (integer)

- quorum:

  Vote threshold a party must reach. Used as fraction of total votes if
  less than 1, otherwise as number of votes.

## Value

The number of seats per party as a vector

## Details

Divisor methods are known under different names:

- d'hondt, jefferson, hagenbach-bischoff: `divisor_floor()`

- sainte-lague, webster: `divisor_round()`

- adams: `divisor_ceiling()`

- dean: `divisor_harmonic()`

- huntington-hill, hill-huntington: `divisor_geometric()`

## See also

[`proporz()`](https://polettif.github.io/proporz/reference/proporz.md),
[`highest_averages_method()`](https://polettif.github.io/proporz/reference/highest_averages_method.md)

## Examples

``` r
votes = c("Party A" = 690, "Party B" = 400,
          "Party C" = 250, "Party D" = 120)

divisor_round(votes, 10)
#> Party A Party B Party C Party D 
#>       4       3       2       1 

divisor_floor(votes, 10)
#> Party A Party B Party C Party D 
#>       5       3       2       0 

divisor_ceiling(votes, 10)
#> Party A Party B Party C Party D 
#>       4       3       2       1 

divisor_ceiling(votes, 5)
#> Party A Party B Party C Party D 
#>       2       1       1       1 

divisor_geometric(votes, 10, quorum = 0.05)
#> Party A Party B Party C Party D 
#>       4       3       2       1 

divisor_harmonic(votes, 10)
#> Party A Party B Party C Party D 
#>       4       3       2       1 
```

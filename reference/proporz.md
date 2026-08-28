# Proportional apportionment

Calculate seat apportionment for legislative bodies.

## Usage

``` r
proporz(votes, n_seats, method, quorum = 0)
```

## Arguments

- votes:

  Numeric vector with the number of votes for each party

- n_seats:

  Total number of seats (integer)

- method:

  Apportionment method to use, as character. Not case sensitive. See
  details.

- quorum:

  Vote threshold a party must reach. Used as fraction of total votes if
  less than 1, otherwise as number of votes.

## Value

The number of seats per party as a vector

## Details

The following methods are available:

- d'hondt, jefferson, hagenbach-bischoff, floor:
  [`divisor_floor()`](https://polettif.github.io/proporz/reference/divisor_methods.md)

- sainte-lague, webster, round:
  [`divisor_round()`](https://polettif.github.io/proporz/reference/divisor_methods.md)

- adams, ceiling:
  [`divisor_ceiling()`](https://polettif.github.io/proporz/reference/divisor_methods.md)

- dean, harmonic:
  [`divisor_harmonic()`](https://polettif.github.io/proporz/reference/divisor_methods.md)

- huntington-hill, hill-huntington, geometric:
  [`divisor_geometric()`](https://polettif.github.io/proporz/reference/divisor_methods.md)

- hare-niemeyer, hamilton, vinton, largest_remainder_method:
  [`largest_remainder_method()`](https://polettif.github.io/proporz/reference/largest_remainder_method.md)

## Note

Seats can also be apportioned among regions instead of parties. The
parameter `votes` is then normally used with census data (e.g.
population counts).

## Examples

``` r
votes = c("Party A" = 651, "Party B" = 349, "Party C" = 50)

proporz(votes, 10, "sainte-lague")
#> Party A Party B Party C 
#>       7       3       0 

proporz(votes, 10, "hill-huntington")
#> Party A Party B Party C 
#>       6       3       1 

proporz(votes, 10, "hill-huntington", quorum = 0.05)
#> Party A Party B Party C 
#>       6       4       0 

proporz(votes, 10, "jefferson", quorum = 70)
#> Party A Party B Party C 
#>       7       3       0 
```

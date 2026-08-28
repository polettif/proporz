# Largest remainder method

Allocate seats based on the largest fractional remainder. The largest
remainder method is also known as Hamilton, Hare-Niemeyer or Vinton
method.

## Usage

``` r
largest_remainder_method(votes, n_seats, quorum = 0)
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

The numbers of votes for each party is divided by a quota representing
the number of votes required for a seat. Then, each party receives the
rounded down quota value as seats. The remaining seats are given to the
party with the largest remainder until all seats have been distributed.

## Note

Only the quota `total votes / total seats` (which is used by the
aforementioned methods) is implemented.

## See also

[`proporz()`](https://polettif.github.io/proporz/reference/proporz.md)

## Examples

``` r
votes = c(47000, 16000, 15800, 12000, 6100, 3100)
largest_remainder_method(votes, 10)
#> [1] 5 2 1 1 1 0
```

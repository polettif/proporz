# Calculate raw seat matrix

Apply row and column divisors to matrix to get non-rounded seat values.
Resulting `NA`, `NaN` or `Inf` values are set to zero.

## Usage

``` r
divide_votes_matrix(M, col_divisors, row_divisors)
```

## Arguments

- M:

  Matrix

- col_divisors:

  Divisors to apply to columns

- row_divisors:

  Divisors to apply to rows

## Value

A matrix with the same dimension as `M` containing non-rounded seat
values

## Note

This is an internal function for
[`lower_apportionment()`](https://polettif.github.io/proporz/reference/lower_apportionment.md).

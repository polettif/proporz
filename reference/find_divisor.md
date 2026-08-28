# Find divisor to assign seats

Find a divisor between `divisor_from` and `divisor_to` such that
`sum(round_func(votes/divisor))` equals `target_seats`

## Usage

``` r
find_divisor(votes, divisor_from, divisor_to, target_seats, round_func)
```

## Arguments

- votes:

  Votes (matrix with only one column or row, allows to use row/colnames
  within `round_func`)

- divisor_from:

  Lower bound for divisor search range (is decreased if necessary)

- divisor_to:

  Upper bound for divisor search range (is increased if necessary)

- target_seats:

  Number of seats to distribute (single number)

- round_func:

  Rounding function

## Value

divisor (single number)

## Note

This is an internal function for
[`find_matrix_divisors()`](https://polettif.github.io/proporz/reference/find_matrix_divisors.md).

# Find divisors for a matrix with alternate scaling

Find divisors for a matrix with alternate scaling

## Usage

``` r
find_matrix_divisors(M, seats_cols, seats_rows, round_func)
```

## Arguments

- M:

  votes_matrix

- seats_cols:

  Target seats for each column

- seats_rows:

  Target seats for each row

- round_func:

  Rounding function. Called like
  `round_func(M/row_divisors/col_divisors)`, divisors are applied
  row/col-wise with
  [`divide_votes_matrix()`](https://polettif.github.io/proporz/reference/divide_votes_matrix.md).

## Value

list of divisors (column and row)

## Note

This is an internal function for
[`lower_apportionment()`](https://polettif.github.io/proporz/reference/lower_apportionment.md).

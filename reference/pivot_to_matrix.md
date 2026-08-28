# Pivot long data.frame to wide matrix and vice versa

Create a matrix in 'wide' format from a data.frame with 3 columns with
`pivot_to_matrix` or create a data.frame in long format from a matrix
with `pivot_to_df.`

## Usage

``` r
pivot_to_matrix(df_long)

pivot_to_df(matrix_wide, value_colname = "values")
```

## Arguments

- df_long:

  `data.frame` in long format with exactly 3 columns

- matrix_wide:

  Matrix in wide format

- value_colname:

  Name for the new value column in the resulting data.frame

## Value

A data.frame with 3 columns or a matrix. Note that the results are
sorted by the first and second column (data.frame) or row/column names
(matrix).

## Details

These pivot functions are used to prepare data for
[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
in
[`pukelsheim()`](https://polettif.github.io/proporz/reference/pukelsheim.md).
They are not supposed to cover general use cases or provide
customization. They mainly exist because reshape is hard to handle and
the package should have no dependencies.

## Examples

``` r
# From data.frame to matrix
df = data.frame(party = c("A", "A", "A", "B", "B", "B"),
                region = c("III", "II", "I", "I", "II", "III"),
                seats = c(5L, 3L, 1L, 2L, 4L, 6L))
pivot_to_matrix(df)
#>      region
#> party I II III
#>     A 1  3   5
#>     B 2  4   6

# from matrix to data.frame
mtrx = matrix(1:6, nrow = 2)
pivot_to_df(mtrx)
#>   row col values
#> 1   1   1      1
#> 2   1   2      3
#> 3   1   3      5
#> 4   2   1      2
#> 5   2   2      4
#> 6   2   3      6

# from matrix to data.frame using dimnames
dimnames(mtrx) <- list(party = c("A", "B"), region = c("I", "II", "III"))
pivot_to_df(mtrx, "seats")
#>   party region seats
#> 1     A      I     1
#> 2     A     II     3
#> 3     A    III     5
#> 4     B      I     2
#> 5     B     II     4
#> 6     B    III     6

# pivot results are sorted
pivot_to_df(pivot_to_matrix(df)) == df[order(df[[1]], df[[2]]),]
#>      party region values
#> [1,]  TRUE   TRUE   TRUE
#> [2,]  TRUE   TRUE   TRUE
#> [3,]  TRUE   TRUE   TRUE
#> [4,]  TRUE   TRUE   TRUE
#> [5,]  TRUE   TRUE   TRUE
#> [6,]  TRUE   TRUE   TRUE
```

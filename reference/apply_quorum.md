# Apply quorum to votes vector or matrix

This quorum calculation implementation is called within
[`proporz()`](https://polettif.github.io/proporz/reference/proporz.md),
[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
and related functions. Generally, there is no need to call
`apply_quorum` directly.

## Usage

``` r
apply_quorum(votes, quorum)
```

## Arguments

- votes:

  Votes vector or votes matrix

- quorum:

  Depending on `votes`:

  - For a vector: Vote threshold a party must reach. Used as fraction of
    total votes if less than 1 otherwise as number of votes.

  - For a matrix: List of quorum functions (created with
    [`quorum_functions`](https://polettif.github.io/proporz/reference/quorum_functions.md))
    or a (named) logical vector with the same length as the number of
    `votes` rows.

## Value

Vector or matrix with same dimension as `votes`. Parties that failed to
reach the specified quorum have their votes set to zero.

## See also

[`quorum_functions`](https://polettif.github.io/proporz/reference/quorum_functions.md)
for more matrix examples.

## Examples

``` r
# vector
(votes <- c(81, 9, 10))
#> [1] 81  9 10

apply_quorum(votes, 10)
#> [1] 81  0 10

apply_quorum(votes, .11)
#> [1] 81  0  0

# matrix
(votes_matrix <- matrix(c(91, 9, 199, 1), nrow = 2))
#>      [,1] [,2]
#> [1,]   91  199
#> [2,]    9    1

apply_quorum(votes_matrix, quorum_all(total = 0.1))
#>      [,1] [,2]
#> [1,]   91  199
#> [2,]    0    0

apply_quorum(votes_matrix, c(FALSE, TRUE))
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    9    1

# named matrix needs a named vector
rownames(votes_matrix) <- c("A", "B")
quorum_vec = c(B = TRUE, A = FALSE)
apply_quorum(votes_matrix, quorum_vec)
#>   [,1] [,2]
#> A    0    0
#> B    9    1
```

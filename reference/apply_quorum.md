# Apply quorum to votes vector or matrix

This quorum calculation implementation is called within
[`proporz()`](https://polettif.github.io/proporz/reference/proporz.md),
there's generally no need to call it directly.

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

  - For a matrix: List of quorum parameters (created with
    [`quorum_functions`](https://polettif.github.io/proporz/reference/quorum_functions.md))
    or a (named) logical vector with the same length as the number of
    `votes` rows. To prevent potential misapplications, using
    `total_voters` or `any_district_voters`, which might require a
    weighted votes matrix, is not permitted. Please use
    [`eval_quorum()`](https://polettif.github.io/proporz/reference/eval_quorum.md)
    in such cases.

## Value

Vector or matrix with same dimension as `votes`. Parties that failed to
reach the specified quorum have their votes set to zero.

## See also

This method is semi-deprecated,
[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
uses
[`eval_quorum()`](https://polettif.github.io/proporz/reference/eval_quorum.md)
and
[`zero_failed_quorum()`](https://polettif.github.io/proporz/reference/zero_failed_quorum.md).

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

apply_quorum(votes_matrix, quorum_all(votes_total = 0.1))
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

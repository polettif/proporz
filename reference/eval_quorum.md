# Evaluate a quorum

Check which parties (votes matrix rows) reached the quorum
specifications and thresholds given by
[`quorum_any()`](https://polettif.github.io/proporz/reference/quorum_functions.md)
or
[`quorum_all()`](https://polettif.github.io/proporz/reference/quorum_functions.md).

## Usage

``` r
eval_quorum(quorum, votes_matrix, weighted_votes_matrix)
```

## Arguments

- quorum:

  Quorum parameter list created by
  [`quorum_any()`](https://polettif.github.io/proporz/reference/quorum_functions.md)
  or
  [`quorum_all()`](https://polettif.github.io/proporz/reference/quorum_functions.md).
  It is also possible to pass a pre-calculated logical vector (without
  `NA`) indicating which parties meet the quorum. The vector is
  name-matched to `votes_matrix`.

- votes_matrix:

  A matrix of unweighted votes. Required when the quorum uses
  `votes_any_district` or `votes_total`, and when `quorum` is a logical
  vector.

- weighted_votes_matrix:

  A matrix of weighted votes (voters). Required when the quorum uses
  `any_district_voters` or `total_voters`.

## Value

A logical vector indicating which parties meet the quorum.

## See also

[`zero_failed_quorum()`](https://polettif.github.io/proporz/reference/zero_failed_quorum.md)

## Examples

``` r
votes_matrix = matrix(c(5020, 550, 800, 100, 1040, 550, 0, 10), ncol = 2)
dimnames(votes_matrix) <- list(c("A", "B", "C", "D"), c("Z1", "Z2"))
seats = c(Z1 = 50, Z2 = 20)

quorum1 = quorum_all(votes_any_district = 0.1)
eval_quorum(quorum1, votes_matrix)
#>     A     B     C     D 
#>  TRUE  TRUE  TRUE FALSE 

quorum2 = quorum_all(votes_any_district = 0.1, total_voters = 20)
votes_matrix_w = weight_votes_matrix(votes_matrix, seats)
eval_quorum(quorum2, votes_matrix, votes_matrix_w)
#>     A     B     C     D 
#>  TRUE  TRUE FALSE FALSE 
```

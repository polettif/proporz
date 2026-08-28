# Check if parties reached the quorum for all votes

Base implementation, used by
[`quorum_functions`](https://polettif.github.io/proporz/reference/quorum_functions.md).

## Usage

``` r
reached_quorum_total(votes_matrix, quorum_total)
```

## Arguments

- votes_matrix:

  Votes matrix

- quorum_total:

  Vote threshold a party must reach for all votes cast. Used as fraction
  of total votes if less than 1, otherwise as number of votes. Must be
  greater than 0.

## Value

Logical vector with length equal to the number of lists/parties
(`votes_matrix` rows) showing whether they reached the quorum or not.

## Note

Votes are not weighted across districts. This is relevant if the quorum
threshold is the minimal number of *voters* (either as percentage or
absolute value). In this case, use
[`weight_votes_matrix()`](https://polettif.github.io/proporz/reference/weight_votes_matrix.md)
before calculating the quorum.

## See also

[`reached_quorum_any_district()`](https://polettif.github.io/proporz/reference/reached_quorum_any_district.md)

## Examples

``` r
(vm <- matrix(c(239, 10, 308, 398, 20, 925), nrow = 3))
#>      [,1] [,2]
#> [1,]  239  398
#> [2,]   10   20
#> [3,]  308  925
reached_quorum_total(vm, 35)
#> [1]  TRUE FALSE  TRUE
```

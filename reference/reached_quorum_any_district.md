# Check if parties reached a quorum in at least one district

Base implementation, used by
[`quorum_functions`](https://polettif.github.io/proporz/reference/quorum_functions.md).

## Usage

``` r
reached_quorum_any_district(votes_matrix, quorum_districts)
```

## Arguments

- votes_matrix:

  Votes matrix

- quorum_districts:

  Vote threshold a party must reach in *at least* one district. Used as
  fraction of total votes within a district if less than 1, otherwise as
  number of votes. Must be greater than 0.

## Value

Logical vector with length equal to the number of lists/parties
(`votes_matrix` rows) showing whether they reached the quorum or not.

## See also

[`reached_quorum_total()`](https://polettif.github.io/proporz/reference/reached_quorum_total.md)

## Examples

``` r
(vm <- matrix(c(239, 10, 308, 398, 20, 925), nrow = 3))
#>      [,1] [,2]
#> [1,]  239  398
#> [2,]   10   20
#> [3,]  308  925
reached_quorum_any_district(vm, 25)
#> [1]  TRUE FALSE  TRUE
```

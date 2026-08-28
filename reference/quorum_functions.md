# Create quorum functions for biproportional apportionment

`quorum_any()` and `quorum_all()` are used for the `quorum` parameter in
[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)/[`pukelsheim()`](https://polettif.github.io/proporz/reference/pukelsheim.md)
and help describe how quorums should be applied prior to seat
distributions.

## Usage

``` r
quorum_all(any_district, total)

quorum_any(any_district, total)
```

## Arguments

- any_district:

  Vote threshold a party must reach in *at least* one district. Used as
  share of total votes within a district if less than 1, otherwise as
  number of votes. Must be \> 0 if provided. Uses
  [`reached_quorum_any_district()`](https://polettif.github.io/proporz/reference/reached_quorum_any_district.md).

- total:

  Vote threshold a party must reach for all votes cast. Used as share of
  total votes if less than 1, otherwise as number of votes. Note that
  votes are not weighted with
  [`weight_votes_matrix()`](https://polettif.github.io/proporz/reference/weight_votes_matrix.md)
  across districts. Must be \> 0 if provided. Uses
  [`reached_quorum_total()`](https://polettif.github.io/proporz/reference/reached_quorum_total.md).

## Value

A function which, when called with `function(votes_matrix)`, returns a
boolean vector with length equal to the number of lists/parties
(`votes_matrix` rows). The vector shows whether a party has reached
any/all quorums.

## Details

There is a difference in how the two functions work. With `quorum_any`,
*at least one* quorum must be reached. With `quorum_all` *all* (i.e.
both) quorums must be reached. If you only use one parameter,
`quorum_any()` and `quorum_all()` are identical.

## See also

[`apply_quorum()`](https://polettif.github.io/proporz/reference/apply_quorum.md)
for standalone quorum calculations

## Examples

``` r
votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)
dimnames(votes_matrix) <- list(c("A", "B", "C", "D"), c("Z1", "Z2"))
seats = c(Z1 = 50, Z2 = 20)

# use as parameter in biproporz or pukelsheim (general use case)
biproporz(votes_matrix, seats,
          quorum = quorum_any(any_district = 0.1, total = 100))
#>   Z1 Z2
#> A 40 12
#> B  5  8
#> C  5  0
#> D  0  0

biproporz(votes_matrix, seats,
          quorum = quorum_all(any_district = 0.1, total = 100))
#>   Z1 Z2
#> A 44 12
#> B  6  8
#> C  0  0
#> D  0  0

biproporz(votes_matrix, seats, quorum = quorum_any(any_district = 0.1))
#>   Z1 Z2
#> A 40 12
#> B  5  8
#> C  5  0
#> D  0  0

biproporz(votes_matrix, seats, quorum = quorum_any(total = 100))
#>   Z1 Z2
#> A 44 12
#> B  6  8
#> C  0  0
#> D  0  0

biproporz(votes_matrix, seats, quorum = quorum_any(total = 0.5))
#>   Z1 Z2
#> A 50 20
#> B  0  0
#> C  0  0
#> D  0  0

# the quorum parameter also accepts named vectors (e.g. calculated elsewhere)
biproporz(votes_matrix, seats, quorum = c(A = FALSE, B = TRUE, C = TRUE, D = TRUE))
#>   Z1 Z2
#> A  0  0
#> B 27 20
#> C 20  0
#> D  3  0
```

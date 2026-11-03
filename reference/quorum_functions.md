# Create quorum parameter list for biproportional apportionment

`quorum_any()` and `quorum_all()` are used for the `quorum` parameter in
[`upper_apportionment()`](https://polettif.github.io/proporz/reference/upper_apportionment.md)
(and thus
[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)/[`pukelsheim()`](https://polettif.github.io/proporz/reference/pukelsheim.md))
and describe how quorums should be applied prior to seat distributions.

## Usage

``` r
quorum_all(
  votes_any_district,
  votes_total,
  any_district_voters,
  total_voters,
  total = NULL,
  any_district = NULL
)

quorum_any(
  votes_any_district,
  votes_total,
  any_district_voters,
  total_voters,
  total = NULL,
  any_district = NULL
)
```

## Arguments

- votes_any_district:

  Vote threshold a party must reach in *at least* one district. Used as
  share of total votes within a district if less than 1, otherwise as
  number of votes. Must be \> 0 if provided. Uses
  [`reached_quorum_any_district()`](https://polettif.github.io/proporz/reference/reached_quorum_any_district.md).

- votes_total:

  Vote threshold a party must reach for all votes cast. Used as share of
  total votes if less than 1, otherwise as number of votes. Note that
  votes are not weighted with
  [`weight_votes_matrix()`](https://polettif.github.io/proporz/reference/weight_votes_matrix.md)
  across districts, the usual and theoretically sound application for a
  total quorum is `total_voters`. Must be \> 0 if provided. Uses
  [`reached_quorum_total()`](https://polettif.github.io/proporz/reference/reached_quorum_total.md).

- any_district_voters:

  Weighted vote (voters) threshold a party must reach in *at least* one
  district. Used as share of total weighted votes (voters) within a
  district if less than 1, otherwise as number of voters. Must be \> 0
  if provided. Uses
  [`reached_quorum_any_district()`](https://polettif.github.io/proporz/reference/reached_quorum_any_district.md)
  with a weighted votes matrix.

- total_voters:

  Vote threshold a party must reach for all weighted votes (voters).
  Used as share of total weighted votes (voters) if less than 1,
  otherwise as number of voters. Must be \> 0 if provided. Uses
  [`reached_quorum_total()`](https://polettif.github.io/proporz/reference/reached_quorum_total.md)
  with a weighted votes matrix.

- total:

  Replaced by `total_voters` (recommended) or `votes_total`

- any_district:

  Replaced by `votes_any_district` (recommended) or
  `any_district_voters`

## Value

List of parameters that describe which quorum methods and threshold
values should be used. The list is handled in
[`upper_apportionment()`](https://polettif.github.io/proporz/reference/upper_apportionment.md)
by
[`eval_quorum()`](https://polettif.github.io/proporz/reference/eval_quorum.md).

## Details

There is a difference in how the two functions work. With `quorum_any`,
*at least one* quorum must be reached. With `quorum_all` *all* quorums
must be reached. If you only use one parameter, `quorum_any()` and
`quorum_all()` are identical.

## Examples

``` r
votes_matrix = matrix(c(5020, 550, 800, 100, 1040, 550, 0, 10), ncol = 2)
dimnames(votes_matrix) <- list(c("A", "B", "C", "D"), c("Z1", "Z2"))
seats = c(Z1 = 50, Z2 = 20)

# use as parameter in biproporz or pukelsheim (general use case)
biproporz(votes_matrix, seats,
          quorum = quorum_any(votes_any_district = 0.1, total_voters = 20))
#>   Z1 Z2
#> A 40 12
#> B  5  8
#> C  5  0
#> D  0  0

biproporz(votes_matrix, seats,
          quorum = quorum_all(votes_any_district = 0.1, total_voters = 20))
#>   Z1 Z2
#> A 44 12
#> B  6  8
#> C  0  0
#> D  0  0

biproporz(votes_matrix, seats, quorum = quorum_any(votes_any_district = 0.1))
#>   Z1 Z2
#> A 40 12
#> B  5  8
#> C  5  0
#> D  0  0

biproporz(votes_matrix, seats, quorum = quorum_any(total_voters = 0.05))
#>   Z1 Z2
#> A 40 12
#> B  5  8
#> C  5  0
#> D  0  0

# the quorum parameter also accepts named vectors (e.g. calculated elsewhere)
biproporz(votes_matrix, seats,
          quorum = c(A = FALSE, B = TRUE, C = TRUE, D = TRUE))
#>   Z1 Z2
#> A  0  0
#> B 27 20
#> C 20  0
#> D  3  0

# quorum specifications are checked with eval_quorum
eval_quorum(quorum_any(votes_any_district = 0.2), votes_matrix)
#>     A     B     C     D 
#>  TRUE  TRUE FALSE FALSE 
```

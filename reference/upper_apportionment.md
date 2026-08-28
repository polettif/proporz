# Upper apportionment

In the first step of biproportional apportionment parties are given
seats according to the sum of their votes across all districts.

## Usage

``` r
upper_apportionment(
  votes_matrix,
  district_seats,
  weight_votes = TRUE,
  method = "round"
)
```

## Arguments

- votes_matrix:

  Vote count matrix with votes by party in rows and votes by district in
  columns.

- district_seats:

  - Vector defining the number of seats per district. Must be the same
    length as `ncol(votes_matrix)`. Values are name-matched to
    `votes_matrix` columns if both are named.

  - If the number of seats per district should be calculated according
    to the number of votes, a single number for the total number of
    seats can be used. `weight_votes` will be ignored, as `votes_matrix`
    shows the number of voters. Note that apportioning district seats
    according to votes is not the general use case.

- weight_votes:

  By default (`TRUE`) it is assumed that each voter in a district has as
  many votes as there are seats in a district. Thus, votes are weighted
  according to the number of available district seats with
  [`weight_votes_matrix()`](https://polettif.github.io/proporz/reference/weight_votes_matrix.md).
  Set to `FALSE` if `votes_matrix` shows the number of *voters* (e.g.
  because they can only cast one vote for one party).

- method:

  Apportion method that defines how seats are assigned, see
  [`proporz()`](https://polettif.github.io/proporz/reference/proporz.md).
  Default is the Sainte-Laguë/Webster method.

## Value

A named list with `district` seats (for `votes_matrix` columns) and
`party` seats (for rows).

## Note

The results from the upper apportionment define the number of seats for
each party and the number of seats for each district for the whole
voting area. The lower apportionment will only determine where (i.e.
which district) the party seats are allocated. Thus, after the upper
apportionment is done, the final strength of a party/district within the
parliament is definite.

## See also

[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md),
[`lower_apportionment()`](https://polettif.github.io/proporz/reference/lower_apportionment.md)

## Examples

``` r
votes_matrix = matrix(c(123,912,312,45,714,255,815,414,215), nrow = 3)
district_seats = c(7,5,8)

upper_apportionment(votes_matrix, district_seats)
#> $district
#> [1] 7 5 8
#> 
#> $party
#> [1]  5 11  4
#> 
```

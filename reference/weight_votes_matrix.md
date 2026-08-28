# Create weighted votes matrix

Weight votes by dividing the votes matrix entries by the number of seats
per district. Votes in districts without seats are set to zero.

## Usage

``` r
weight_votes_matrix(votes_matrix, district_seats)

weight_list_votes(votes_matrix, district_seats)
```

## Arguments

- votes_matrix:

  Votes matrix

- district_seats:

  Seats per district, vector with same length as `ncol(votes_matrix)`
  and names as `colnames(votes_matrix)`

## Value

The weighted `votes_matrix` with the number of voters (not rounded).

## Note

`weight_list_votes()` has been renamed to `weight_votes_matrix()` in
`v1.5.2` and is deprecated.

## See also

This method is used in
[`upper_apportionment()`](https://polettif.github.io/proporz/reference/upper_apportionment.md)
if `weight_votes` is `TRUE` (default).

## Examples

``` r
(vm <- matrix(c(20, 10, 8, 5), nrow = 2))
#>      [,1] [,2]
#> [1,]   20    8
#> [2,]   10    5
weight_votes_matrix(vm, c(3, 2))
#>          [,1] [,2]
#> [1,] 6.666667  4.0
#> [2,] 3.333333  2.5

weight_votes_matrix(uri2020$votes_matrix, uri2020$seats_vector)
#>       Altdorf  Bürglen Erstfeld Schattdorf
#> CVP  764.7333 403.1429 384.8333   532.6667
#> SPGB 793.8667 229.4286 284.1667   288.8889
#> FDP  614.2000 223.8571 157.6667   329.0000
#> SVP  517.0667 420.7143 262.1667   388.6667
```

# Find which party has the most votes in a district

Create a logical matrix that shows whether a party got the most votes in
a district or not.

## Usage

``` r
district_winner_matrix(votes_matrix, district_seats = 1L)
```

## Arguments

- votes_matrix:

  Vote count matrix with votes by party in rows and votes by district in
  columns.

- district_seats:

  Vector defining the number of seats per district. Must be the same
  length as `ncol(votes_matrix)`. Values are name-matched to
  `votes_matrix` columns if both are named. If a single value is
  supplied (like `1` as default), it is used as the number of seats for
  every district.

## Value

Logical matrix with the same dimensions and names as `votes_matrix`

## Details

If two or more parties are tied and there are not enough seats for each
tied party, the matrix value is `NA`.

## Examples

``` r
(vm <- matrix(c(60,30,0,20,10,30),
              nrow = 3, dimnames = list(1:3, c("A", "B"))))
#>    A  B
#> 1 60 20
#> 2 30 10
#> 3  0 30

district_winner_matrix(vm)
#>       A     B
#> 1  TRUE FALSE
#> 2 FALSE FALSE
#> 3 FALSE  TRUE

# NA values if parties are tied (here in district B)
vm[1,2] <- 30
district_winner_matrix(vm)
#>       A     B
#> 1  TRUE    NA
#> 2 FALSE FALSE
#> 3 FALSE    NA

# No NA values for tied parties if enough seats are available
district_winner_matrix(vm, c(1, 2))
#>       A     B
#> 1  TRUE  TRUE
#> 2 FALSE FALSE
#> 3 FALSE  TRUE
```

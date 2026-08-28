# Lower apportionment

In the second biproportional apportionment step, party and district
divisors are calculated such that the row and column sums of the
resulting seats matrix satisfy the constraints given by the upper
apportionment.

## Usage

``` r
lower_apportionment(votes_matrix, seats_cols, seats_rows, method = "round")
```

## Arguments

- votes_matrix:

  Matrix with votes by party in rows and votes by district in columns.

- seats_cols:

  Number of seats per column (districts/regions), predetermined or
  calculated with
  [`upper_apportionment()`](https://polettif.github.io/proporz/reference/upper_apportionment.md).

- seats_rows:

  Number of seats per row (parties/lists), calculated with
  [`upper_apportionment()`](https://polettif.github.io/proporz/reference/upper_apportionment.md).

- method:

  Apportion method that defines how seats are assigned. The following
  methods are supported:

  - `round`: The default Sainte-Laguë/Webster method is the standard for
    biproportional apportionment and the only method guaranteed to
    terminate.

  - `wto`: "winner take one" works like `round` with a condition that
    the party that got the most votes in a district must get *at least*
    one seat ('Majorzbedingung', also called 'strongest party
    constrained' rule (SPC)). `votes_matrix` must have row and column
    names to use this method. A district winner can only get a seat if
    they are entitled to one from the upper apportionment
    (`seats_rows`). The condition does not apply in a district if two or
    more parties have the same number of votes and there are not enough
    seats for these parties. A warning is issued in this case. Modify
    the votes matrix to explicitly break ties.

  - You can provide a custom function that rounds a matrix (i.e. the the
    `votes_matrix` divided by party and district divisors) without
    further parameters.

  - It is possible to use any divisor method name listed in
    [`proporz()`](https://polettif.github.io/proporz/reference/proporz.md).

## Value

A seat matrix with district (columns) and party (rows) divisors stored
in attributes.

## Details

The result is obtained by an iterative process ('Alternate Scaling
Algorithm', see Reference). Initially, for each district a divisor is
chosen using the highest averages method for the votes allocated to each
regional party list in this region. For each party a party divisor is
initialized with 1.

Effectively, the objective of the iterative process is to modify the
regional divisors and party divisors so that the number of seats in each
regional party list equals the number of their votes divided by both the
regional and the party divisors.

The following two correction steps are executed until this objective is
satisfied:

- modify the party divisors such that the apportionment within each
  party is correct with the chosen rounding method,

- modify the regional divisors such that the apportionment within the
  region is correct with the chosen rounding method.

## Note

If the maximum number of optimization iterations is reached, an error is
thrown since no solution can be found. You can overwrite the default
(1000) with `options(proporz_max_iterations = ...)` but it is very
likely that the result is undefined given the structure of the input
parameters.

## References

Oelbermann, K. F. (2016): Alternate scaling algorithm for biproportional
divisor methods. Mathematical Social Sciences, 80, 25-32.

## See also

[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md),
[`upper_apportionment()`](https://polettif.github.io/proporz/reference/upper_apportionment.md),
[`district_winner_matrix()`](https://polettif.github.io/proporz/reference/district_winner_matrix.md)

## Examples

``` r
votes_matrix = matrix(c(123,912,312,45,714,255,815,414,215), nrow = 3)
district_seats = c(7,5,8)
party_seats = c(5,11,4)

lower_apportionment(votes_matrix, district_seats, party_seats)
#>      [,1] [,2] [,3]
#> [1,]    1    0    4
#> [2,]    4    4    3
#> [3,]    2    1    1
#> attr(,"divisors")
#> attr(,"divisors")$districts
#> [1] 204 180 162
#> 
#> attr(,"divisors")$parties
#> [1] 1.2 1.0 1.0
#> 


# using "winner take one"
vm = matrix(c(200,100,10,11), 2,
            dimnames = list(c("Party A", "Party B"), c("I", "II")))
district_seats = setNames(c(2,1), colnames(vm))
ua = upper_apportionment(vm, district_seats)

lower_apportionment(vm, ua$district, ua$party, method = "wto")
#>         I II
#> Party A 2  0
#> Party B 0  1
#> attr(,"divisors")
#> attr(,"divisors")$districts
#>   I  II 
#> 144  31 
#> 
#> attr(,"divisors")$parties
#> Party A Party B 
#>     0.7     2.0 
#> 

# compare to standard method
lower_apportionment(vm, ua$district, ua$party, method = "round")
#>         I II
#> Party A 1  1
#> Party B 1  0
#> attr(,"divisors")
#> attr(,"divisors")$districts
#>   I  II 
#> 150  21 
#> 
#> attr(,"divisors")$parties
#> Party A Party B 
#>     0.9     1.2 
#> 
```

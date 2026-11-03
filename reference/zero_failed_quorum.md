# Set votes of parties failing to reach quorums to zero

This is a convenience function for `M[!quorum_vector, ] <- 0L`. Used in
combination with
[`eval_quorum()`](https://polettif.github.io/proporz/reference/eval_quorum.md)
in
[`upper_apportionment()`](https://polettif.github.io/proporz/reference/upper_apportionment.md).

## Usage

``` r
zero_failed_quorum(M, quorum_vector)
```

## Arguments

- M:

  votes or voter matrix

- quorum_vector:

  logical vector with length equal to `nrow(M)`.

## Value

`M` with party rows set to zero

## See also

[`eval_quorum()`](https://polettif.github.io/proporz/reference/eval_quorum.md)

## Examples

``` r
(vm <- matrix(c(30, 20, 20, 50), 2))
#>      [,1] [,2]
#> [1,]   30   20
#> [2,]   20   50
(b <- eval_quorum(quorum_any(votes_any_district = 40), vm))
#> [1] FALSE  TRUE

zero_failed_quorum(vm, b)
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]   20   50
```

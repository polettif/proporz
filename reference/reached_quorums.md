# Apply a list of quorum functions to rows in a votes matrix

Apply a list of quorum functions to rows in a votes matrix

## Usage

``` r
reached_quorums(votes_matrix, quorum_funcs)
```

## Arguments

- votes_matrix:

  Votes matrix

- quorum_funcs:

  A function or a list of quorum functions. For lists, the attribute
  "type" must be set which indicates whether `ALL` or `ANY` (i.e. at
  least one) quorums must be reached.

## Value

Logical vector with length equal to the number of lists/parties
(`votes_matrix` rows) showing whether they reached the quorum or not.

## Note

This is an internal function for quorum calculations in
[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md).

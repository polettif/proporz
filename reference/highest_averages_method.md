# Highest averages method

Allocate seats proportionally for
[`divisor methods`](https://polettif.github.io/proporz/reference/divisor_methods.md).

## Usage

``` r
highest_averages_method(votes, n_seats, divisors)
```

## Arguments

- votes:

  Numeric vector with the number of votes for each party

- n_seats:

  Total number of seats (integer)

- divisors:

  Sequence of divisors (length equal to the number of seats). If it is a
  single number (e.g. 0.5), a sequence is generated starting with it,
  increasing by 1.

## Value

The number of seats per party as a vector

## Details

The highest averages method requires the number of votes for each party
to be divided successively by a series of divisors. This produces a
table of quotients, or averages, with a row for each divisor and a
column for each party. The nth seat is allocated to the party whose
column contains the nth largest entry in this table, up to the total
number of seats available.
([Wikipedia](https://en.wikipedia.org/wiki/Highest_averages_method))

## Examples

``` r
highest_averages_method(c(5200, 1700, 3100), 15, 0.5)
#> [1] 8 2 5

highest_averages_method(votes = c(A = 50, B = 0, C = 30),
                        n_seats = 3,
                        divisors = c(0, 1.3333, 2.4))
#> A B C 
#> 2 0 1 
```

# Get district and party divisors from biproporz result

Show the district and party divisors used to assign seats. This method
provides easier access to divisors stored in `attributes(...)$divisors`.

## Usage

``` r
get_divisors(biproporz_result)
```

## Arguments

- biproporz_result:

  A matrix created by
  [`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
  or a data.frame created by
  [`pukelsheim()`](https://polettif.github.io/proporz/reference/pukelsheim.md)

## Value

The district and party divisors (named "districts" and "parties") in a
list, each as a vector

## Examples

``` r
seats_matrix = biproporz(uri2020$votes_matrix, uri2020$seats_vector)
get_divisors(seats_matrix)
#> $districts
#>    Altdorf    Bürglen   Erstfeld Schattdorf 
#>       2689       1194       1088       1539 
#> 
#> $parties
#>   CVP  SPGB   FDP   SVP 
#> 0.946 1.000 1.000 0.970 
#> 

seats_df = pukelsheim(pivot_to_df(uri2020$votes_matrix),
                      data.frame(names(uri2020$seats_vector),
                      uri2020$seats_vector))

get_divisors(seats_df)
#> $districts
#>    Altdorf    Bürglen   Erstfeld Schattdorf 
#>       2689       1194       1088       1539 
#> 
#> $parties
#>   CVP   FDP  SPGB   SVP 
#> 0.946 1.000 1.000 0.970 
#> 

# summary() also prints the divisors for a biproporz matrix
summary(seats_matrix)
#>            Altdorf Bürglen Erstfeld Schattdorf (sum) (divisor)
#>        CVP       5       2        2          3    12     0.946
#>       SPGB       4       1        2          2     9         1
#>        FDP       3       1        1          2     7         1
#>        SVP       3       3        1          2     9      0.97
#>      (sum)      15       7        6          9    37          
#>  (divisor)    2689    1194     1088       1539                
```

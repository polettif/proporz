# proporz <img src="man/figures/logo.png" align="right" height="138" />

Calculate seat apportionment for legislative bodies with various methods. These
methods include divisor methods (e.g. D'Hondt, Webster or Adams), largest 
remainder methods and biproportional apportionment.

_Mit diesem R-Package können mittels verschiedener Sitzzuteilungsverfahren 
Wählerstimmen in Abgeordnetensitze umgerechnet werden. Das Package beinhaltet 
Quoten-, Divisor- und biproportionale Verfahren (Doppelproporz oder 
"Doppelter Pukelsheim")._

## Installation
Install the package from CRAN:

```r
install.packages("proporz")
```

Alternatively, install the development version from Github:

```r
# install.packages("remotes")
remotes::install_github("polettif/proporz")
```

## Apportionment methods overview

### Proportional Apportionment

[`proporz()`](https://polettif.github.io/proporz/reference/proporz.html) distributes 
seats proportionally for a vector of votes according to the following methods:

- **Divisor methods** ([Wikipedia](https://en.wikipedia.org/wiki/Highest_averages_method))
    - D'Hondt, Jefferson, Hagenbach-Bischoff
    - Sainte-Laguë, Webster
    - Adams
    - Dean
    - Huntington-Hill
- **Largest remainder method** ([Wikipedia](https://en.wikipedia.org/wiki/Largest_remainder_method))
    - Hare-Niemeyer, Hamilton, Vinton

``` r
library(proporz)
votes = c("Party A" = 651, "Party B" = 349, "Party C" = 50)

proporz(votes, n_seats = 10, method = "sainte-lague")
#> Party A Party B Party C 
#>       7       3       0

proporz(votes, 10, "huntington-hill", quorum = 0.05)
#> Party A Party B Party C 
#>       6       4       0
```

### Biproportional Apportionment

Biproportional apportionment ([Wikipedia](https://en.wikipedia.org/wiki/Biproportional_apportionment)) 
is a method to proportionally allocate seats among parties and districts.

We can use the provided `zug2018` data set to illustrate biproportional apportionment 
with [`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.html).
You need a 'votes matrix' as input which shows the number of votes for each party
(rows) and district (columns). In this data set, parties are called 'lists' and 
districts 'entities'.

``` r
votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
votes_matrix = pivot_to_matrix(votes_df)
votes_matrix
#>        entity_id
#> list_id  1701 1702 1703 1704 1705 1706 1707 1708 1709 1710  1711
#>       1  2993    0    0    0    0    0    0    0    0    0     0
#>       2  8108 4687 1584  531  279  477 2363 3860 1481   91 22023
#>       3 19389 9334 4807 1946  396 2844 3523 4702 3310  812 21343
#>       4 14814 6691 4005  826  379 1654 2842 2624 2713  461 33789
#>       5  4486 2270  621  198    0  361  728  465  925    0 10131
#>       6 15695 4705 1750   84    0   51  627 1106 1563  302 21794
#>       7 21298 8178 2875 1336  399 1450 3715 2610 4063  344 26798

distr_df = unique(zug2018[c("entity_id", "election_mandates")])
district_seats = setNames(distr_df$election_mandates, distr_df$entity_id)
district_seats
#> 1701 1702 1703 1704 1705 1706 1707 1708 1709 1710 1711 
#>   15   10    6    3    2    4    7    6    6    2   19
```

``` r
biproporz(votes_matrix, district_seats, quorum_any(any_district = 0.05, total = 0.03))
#>        entity_id
#> list_id 1701 1702 1703 1704 1705 1706 1707 1708 1709 1710 1711
#>       1    0    0    0    0    0    0    0    0    0    0    0
#>       2    2    1    1    0    0    0    1    2    1    0    3
#>       3    3    3    2    1    1    2    2    2    1    1    3
#>       4    2    2    1    1    0    1    2    1    1    1    5
#>       5    1    1    0    0    0    0    0    0    0    0    2
#>       6    3    1    1    0    0    0    0    0    1    0    3
#>       7    4    2    1    1    1    1    2    1    2    0    3
```

You can use [`pukelsheim()`](https://polettif.github.io/proporz/reference/pukelsheim.html) 
for data.frames in long format as input data. It is a wrapper for 
`biproporz()`.

``` r
votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
district_seats_df = unique(zug2018[c("entity_id", "election_mandates")])

seats_df = pukelsheim(votes_df,
                      district_seats_df,
                      quorum = quorum_any(any_district = 0.05, total = 0.03))

head(seats_df)
#>   list_id entity_id list_votes seats
#> 1       2      1701       8108     2
#> 2       1      1701       2993     0
#> 3       3      1701      19389     3
#> 4       4      1701      14814     2
#> 5       5      1701       4486     1
#> 6       6      1701      15695     3
```

The [**apportionment scenarios vignette**](https://polettif.github.io/proporz/articles/apportionment_scenarios.html) 
contains more examples.

## Shiny app

The package provides a basic Shiny app where you can calculate biproportional
apportionment on an interactive dashboard. You need to have the packages `shiny` 
and `shinyMatrix` installed.

```r
# install.packages("shiny")
# install.packages("shinyMatrix")
library(proporz)
run_app()
```
<img src="man/figures/shinyapp-example.gif" style = "width:70%;">


## Function details

[**Full function reference**](https://polettif.github.io/proporz/reference/index.html)

#### Divisor methods

You can use divisor methods directly:

``` r
votes = c("Party A" = 690, "Party B" = 370, "Party C" = 210, "Party D" = 10)

# D'Hondt, Jefferson or Hagenbach-Bischoff method
divisor_floor(votes, 10)
#> Party A Party B Party C Party D 
#>       6       3       1       0

# Sainte-Laguë or Webster method
divisor_round(votes, 10)
#> Party A Party B Party C Party D 
#>       5       3       2       0

# Adams method
divisor_ceiling(votes, 10)
#> Party A Party B Party C Party D 
#>       4       3       2       1

# Dean method
divisor_harmonic(votes, 10)
#> Party A Party B Party C Party D 
#>       5       2       2       1

# Huntington-Hill method
divisor_geometric(votes, 10)
#> Party A Party B Party C Party D 
#>       5       3       1       1
```

#### Largest remainder method

The largest remainder method is also accessible directly:

``` r
votes = c("I" = 16200, "II" = 47000, "III" = 12700)

# Hamilton, Hare-Niemeyer or Vinton method
largest_remainder_method(votes, 20)
#>   I  II III 
#>   4  13   3
```

## See also
There are other R packages available that provide apportionment functions, some with
more focus on analysis. However, biproportional apportionment is missing from the 
pure R packages and RBazi needs rJava with an accompanying jar.

- [RBazi](https://www.math.uni-augsburg.de/htdocs/emeriti/pukelsheim/bazi/RBazi.html): Package using rJava to access the functions of [BAZI](https://www.math.uni-augsburg.de/htdocs/emeriti/pukelsheim/bazi/welcome.html).
- [seatdist](https://github.com/jmedzihorsky/seatdist) package for seat apportionment and disproportionality measurement.
- [disprr](https://github.com/pierzgal/disprr) Examine Disproportionality of Apportionment Methods.
- [apportR](https://github.com/jalapic/apportR): Package containing various apportionment methods, with particular relevance for the problem of apportioning seats in the House of Representatives.
- [apportion](https://github.com/christopherkenny/apportion) Convert populations into integer number of seats for legislative bodies, focusing on the United States.

### Contributing

Please feel free to issue a pull request or [open an issue](https://github.com/polettif/proporz/issues/new).

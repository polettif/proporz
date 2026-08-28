# Package index

## Proportional Apportionment

Proportional apportionment distributes seats for a vector of votes. With
*proporz()*, all divisor or largest remainder methods can be accessed.

- [`proporz()`](https://polettif.github.io/proporz/reference/proporz.md)
  : Proportional apportionment

### Divisor Methods

- [`highest_averages_method()`](https://polettif.github.io/proporz/reference/highest_averages_method.md)
  : Highest averages method
- [`divisor_round()`](https://polettif.github.io/proporz/reference/divisor_methods.md)
  [`divisor_floor()`](https://polettif.github.io/proporz/reference/divisor_methods.md)
  [`divisor_harmonic()`](https://polettif.github.io/proporz/reference/divisor_methods.md)
  [`divisor_geometric()`](https://polettif.github.io/proporz/reference/divisor_methods.md)
  [`divisor_ceiling()`](https://polettif.github.io/proporz/reference/divisor_methods.md)
  : Divisor methods

### Largest Remainder Method

- [`largest_remainder_method()`](https://polettif.github.io/proporz/reference/largest_remainder_method.md)
  : Largest remainder method

## Biproportional Apportionment

Biproportional apportionment distributes seats for a matrix of votes
among parties and districts.

- [`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
  : Biproportional apportionment
- [`pukelsheim()`](https://polettif.github.io/proporz/reference/pukelsheim.md)
  : Biproportional apportionment with data frames

### Algorithm implementations

- [`upper_apportionment()`](https://polettif.github.io/proporz/reference/upper_apportionment.md)
  : Upper apportionment
- [`lower_apportionment()`](https://polettif.github.io/proporz/reference/lower_apportionment.md)
  : Lower apportionment

### Quorum calculations

- [`quorum_all()`](https://polettif.github.io/proporz/reference/quorum_functions.md)
  [`quorum_any()`](https://polettif.github.io/proporz/reference/quorum_functions.md)
  : Create quorum functions for biproportional apportionment
- [`apply_quorum()`](https://polettif.github.io/proporz/reference/apply_quorum.md)
  : Apply quorum to votes vector or matrix
- [`reached_quorum_any_district()`](https://polettif.github.io/proporz/reference/reached_quorum_any_district.md)
  : Check if parties reached a quorum in at least one district
- [`reached_quorum_total()`](https://polettif.github.io/proporz/reference/reached_quorum_total.md)
  : Check if parties reached the quorum for all votes

## Shiny App

- [`run_app()`](https://polettif.github.io/proporz/reference/run_app.md)
  : Use biproportional apportionment interactively in a shiny app

## Example Datasets

- [`zug2018`](https://polettif.github.io/proporz/reference/zug2018.md) :
  Election Data for the Cantonal Council of Zug (2018)
- [`uri2020`](https://polettif.github.io/proporz/reference/uri2020.md) :
  Election Data for the Cantonal Council of Uri (2020)
- [`finland2019`](https://polettif.github.io/proporz/reference/finland2019.md)
  : Finnish Parliamentary Elections Data (2019)

## Helpers

- [`get_divisors()`](https://polettif.github.io/proporz/reference/get_divisors.md)
  : Get district and party divisors from biproporz result
- [`weight_votes_matrix()`](https://polettif.github.io/proporz/reference/weight_votes_matrix.md)
  [`weight_list_votes()`](https://polettif.github.io/proporz/reference/weight_votes_matrix.md)
  : Create weighted votes matrix
- [`district_winner_matrix()`](https://polettif.github.io/proporz/reference/district_winner_matrix.md)
  : Find which party has the most votes in a district
- [`pivot_to_matrix()`](https://polettif.github.io/proporz/reference/pivot_to_matrix.md)
  [`pivot_to_df()`](https://polettif.github.io/proporz/reference/pivot_to_matrix.md)
  : Pivot long data.frame to wide matrix and vice versa
- [`ceil_at()`](https://polettif.github.io/proporz/reference/ceil_at.md)
  : Rounding with predefined thresholds

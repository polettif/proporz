# Finnish Parliamentary Elections Data (2019)

Example data from the 2019 Finnish parliamentary elections. The data has
been cleaned up and only contains information relevant for this package.

## Usage

``` r
finland2019
```

## Format

List containing two data.frames:

- `votes_df` containing the number of votes for each party and district.
  229 rows, 3 columns (`party_name`, `district_name`, `votes`)

- `district_seats_df` with the number of seats per district. 12 rows, 2
  columns (`district_name`, `seats`)

## Source

<https://tulospalvelu.vaalit.fi/EKV-2019/en/ladattavat_tiedostot.html>

## Examples

``` r
finland2019$district_seats_df
#>    district_name seats
#> 1            HEL    22
#> 2            HÄM    14
#> 3            KAA    17
#> 4            KES    10
#> 5            LAP     7
#> 6            OUL    18
#> 7            PIR    19
#> 8            SAT     8
#> 9            SKA    15
#> 10           UUS    36
#> 11           VAA    16
#> 12           VAR    17

head(finland2019$votes_df)
#>   party_name district_name  votes
#> 1        KOK           UUS 114243
#> 2        SDP           UUS  97107
#> 3       VIHR           HEL  90662
#> 4         PS           UUS  86691
#> 5        KOK           HEL  84141
#> 6       KESK           OUL  78486
```

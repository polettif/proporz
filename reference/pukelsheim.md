# Biproportional apportionment with data frames

Method to proportionally allocate seats among parties/lists and
districts/regions/entities ('Doppelter Pukelsheim').

## Usage

``` r
pukelsheim(
  votes_df,
  district_seats_df,
  quorum,
  new_seats_col = "seats",
  weight_votes = TRUE,
  winner_take_one = FALSE,
  ...
)
```

## Arguments

- votes_df:

  `data.frame` (long format) with 3 columns (actual colnames can
  differ):

  1.  party id/name (character)

  2.  district id/name (character)

  3.  votes (numeric)

- district_seats_df:

  `data.frame` with 2 columns (actual colnames can differ):

  1.  district id/name (character)

  2.  number of seats for a district (numeric)

- quorum:

  Optional list of functions which take the votes matrix and return a
  logical vector that shows for each party/row whether they reached the
  quorum (i.e. are eligible for seats). The easiest way to do this is by
  passing
  [`quorum_any()`](https://polettif.github.io/proporz/reference/quorum_functions.md)
  or
  [`quorum_all()`](https://polettif.github.io/proporz/reference/quorum_functions.md),
  see examples. Alternatively you can pass a precalculated logical
  vector. No quorum is applied if this parameter is missing or `NULL`.

- new_seats_col:

  Name of the new column

- weight_votes:

  By default (`TRUE`) it is assumed that each voter in a district has as
  many votes as there are seats in a district. Set to `FALSE` if
  `votes_df` shows the number of *voters* (e.g. because they can only
  vote for one party).

- winner_take_one:

  Set to `TRUE` if the party that got the most votes in a district must
  get *at least* one seat ('Majorzbedingung') in this district. This
  only applies if the district winning party is entitled to a seat in
  the upper apportionment. Default is `FALSE`.

- ...:

  Ignored (used for backwards compatibility)

## Value

A data.frame like `votes_df` with a new column denoting the number of
seats per party and district. Party and district divisors stored in
attributes (hidden from print, see
[`get_divisors()`](https://polettif.github.io/proporz/reference/get_divisors.md)).
An ungrouped tibble is returned if `votes_df` is a tibble.

## Details

Each party nominates a candidate list for every district. The voters
vote for the parties of their district. The seat allocation is
calculated in two steps:

1.  In the so called
    [`upper apportionment`](https://polettif.github.io/proporz/reference/upper_apportionment.md)
    the number of seats for each party (over all districts) is
    determined.

2.  In the so called
    [`lower apportionment`](https://polettif.github.io/proporz/reference/lower_apportionment.md)
    the seats are distributed to the regional party list respecting the
    results from the upper apportionment.

Parties failing to reach quorums cannot get seats. This function does
not handle seat assignment to candidates within party lists.

If you want to use other apportion methods than Sainte-Laguë use
[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md).

## See also

This function calls
[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
after preparing the input data.

## Examples

``` r
# Zug 2018
votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
district_seats_df = unique(zug2018[c("entity_id", "election_mandates")])

seats_df = pukelsheim(votes_df,
                      district_seats_df,
                      quorum_any(any_district = 0.05, total = 0.03),
                      winner_take_one = TRUE)

head(seats_df)
#>   list_id entity_id list_votes seats
#> 1       2      1701       8108     2
#> 2       1      1701       2993     0
#> 3       3      1701      19389     3
#> 4       4      1701      14814     2
#> 5       5      1701       4486     1
#> 6       6      1701      15695     3

# Finland 2019
finland19_result = pukelsheim(finland2019$votes_df,
                             finland2019$district_seats_df,
                             new_seats_col = "mandates",
                             weight_votes = FALSE)
tail(finland19_result[order(finland19_result$mandates),])
#>   party_name district_name  votes mandates
#> 4         PS           UUS  86691        5
#> 5        KOK           HEL  84141        5
#> 7       VIHR           UUS  73626        5
#> 2        SDP           UUS  97107        6
#> 6       KESK           OUL  78486        6
#> 1        KOK           UUS 114243        7
```

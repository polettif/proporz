# Apportionment scenarios

## 1) Introduction

This vignette explores the implementation of different seat
apportionment scenarios. There are generally two ways to assign seats of
a parliament: Either you distribute seats within each district
independently or you distribute seats according to the vote share for
the whole election region (e.g. on a national level). We will focus on
the difference in seat shares compared to vote shares these two
approaches can produce.

  

## 2) Setup data

We’ll mainly use the data set for the 2019 Finnish parliamentary
elections. The `finland2019` data set contains two data.frames: One with
the number of votes for each party, the other data.frame tells us how
many seats (`election_mandates`) each district has.

``` r

library(proporz)
str(finland2019)
#> List of 2
#>  $ votes_df         :'data.frame':   229 obs. of  3 variables:
#>   ..$ party_name   : chr [1:229] "KOK" "SDP" "VIHR" "PS" ...
#>   ..$ district_name: chr [1:229] "UUS" "UUS" "HEL" "UUS" ...
#>   ..$ votes        : int [1:229] 114243 97107 90662 86691 84141 78486 73626 66109 59722 55244 ...
#>  $ district_seats_df:'data.frame':   12 obs. of  2 variables:
#>   ..$ district_name: chr [1:12] "HEL" "HÄM" "KAA" "KES" ...
#>   ..$ seats        : int [1:12] 22 14 17 10 7 18 19 8 15 36 ...
```

To make comparisons easier, we’ll use matrices instead of data.frames.
We create a `votes_matrix` from the given data.frame.

``` r

votes_matrix = pivot_to_matrix(finland2019$votes_df)
dim(votes_matrix)
#> [1] 44 12

# Let's look at all parties with at least 10k votes
knitr::kable(votes_matrix[rowSums(votes_matrix) > 10000,])
```

|      |   HÄM |   HEL |   KAA |   KES |   LAP |   OUL |   PIR |   SAT |   SKA |    UUS |   VAA |   VAR |
|:-----|------:|------:|------:|------:|------:|------:|------:|------:|------:|-------:|------:|------:|
| KD   | 11733 |  7253 | 10875 |  8774 |  1077 |  5728 | 17180 |  3361 | 17389 |  14912 | 16741 |  5121 |
| KESK | 20892 | 11015 | 40715 | 30597 | 29145 | 78486 | 26536 | 20878 | 50459 |  35348 | 50053 | 29796 |
| KOK  | 39511 | 84141 | 44224 | 19707 | 11243 | 28382 | 55244 | 17666 | 27699 | 114243 | 29530 | 52367 |
| Nyt  |     0 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |     0 |  24778 |     0 |     0 |
| NYT  |     0 | 13529 |     0 |     0 |     0 |  1593 |     0 |     0 |     0 |      0 |     0 |     0 |
| PIR  |   996 |  5840 |   687 |  1239 |   280 |  1244 |  2018 |   253 |   686 |   4083 |   462 |  1244 |
| PS   | 43199 | 47276 | 46196 | 27851 | 17160 | 52771 | 51819 | 30090 | 39996 |  86691 | 42843 | 52913 |
| RKP  |   500 | 20348 |     0 |     0 |   112 |   432 |   327 |   178 |     0 |  49625 | 52880 | 15238 |
| SDP  | 49359 | 52393 | 59722 | 29201 | 13479 | 26588 | 66109 | 31475 | 38274 |  97107 | 33608 | 49156 |
| SIN  |   821 |  2056 |  3212 |   705 |   483 |  1801 |  3342 |     0 |  4128 |   9498 |  2470 |  1427 |
| STL  |  1028 |  1285 |   829 |   389 |   667 |   847 |  1371 |   199 |   769 |   2754 |   777 |   451 |
| VAS  | 15172 | 42899 | 10371 | 12681 | 14128 | 33820 | 24341 | 12435 | 15908 |  26257 |  8315 | 35481 |
| VIHR | 17354 | 90662 | 23032 | 17639 |  9706 | 20376 | 37152 |  7461 | 21362 |  73626 | 10515 | 25309 |

We also create a named vector that defines how many seats each district
has (`district_seats`). Note that the order of district names in
`district_seats` and columns in `votes_matrix` differ. However, this
does not affect the analysis as we access districts by name (as does
[`biproporz()`](https://polettif.github.io/proporz/reference/biproporz.md)
by the way).

``` r

district_seats = finland2019$district_seats_df$seats
names(district_seats) <- finland2019$district_seats_df$district_name
district_seats
#> HEL HÄM KAA KES LAP OUL PIR SAT SKA UUS VAA VAR 
#>  22  14  17  10   7  18  19   8  15  36  16  17
```

  

## 3) Distribute seats in every district independently

### 3.1) Function to distribute seats in districts

In Finland and many other jurisdictions, seats are assigned for each
district independently. There is no biproportional apportionment among
all districts. The following function, `apply_proporz`, calculates the
seat distribution for each district and returns the number of seats per
party and district as a matrix. The user can specify the apportionment
method and a quorum threshold.

``` r

apply_proporz = function(votes_matrix, district_seats, method, quorum = 0) {
  seats_matrix = votes_matrix
  seats_matrix[] <- NA
  
  # calculate proportional apportionment for each district (matrix column)
  for(district in names(district_seats)) {
    seats_matrix[,district] <- proporz(votes_matrix[,district],
                                       district_seats[district],
                                       quorum = quorum,
                                       method = method)
  }
  return(seats_matrix)
}
```

### 3.2) Actual distribution method (D’Hondt)

The Finnish election system uses the *D’Hondt* method. We’ll calculate
the seat distribution as a baseline to compare it with other methods.

``` r

bydistrict_v0 = apply_proporz(votes_matrix, district_seats, "d'hondt")

bydistrict_v0[rowSums(bydistrict_v0) > 0,]
#>           district_name
#> party_name HÄM HEL KAA KES LAP OUL PIR SAT SKA UUS VAA VAR
#>       KD     1   0   0   0   0   0   1   0   1   1   1   0
#>       KESK   1   0   3   3   3   6   2   1   4   2   4   2
#>       KOK    3   6   3   1   1   2   4   1   2   9   2   4
#>       Nyt    0   0   0   0   0   0   0   0   0   1   0   0
#>       PS     3   3   4   2   1   4   4   2   3   6   3   4
#>       RKP    0   1   0   0   0   0   0   0   0   3   4   1
#>       SDP    4   3   5   2   1   2   5   3   3   7   2   3
#>       VAS    1   3   0   1   1   3   1   1   1   2   0   2
#>       VIHR   1   6   2   1   0   1   2   0   1   5   0   1
```

### 3.3) Alternative methods

We’ll now look at alternative apportionment methods. The *Sainte-Laguë*
method (standard rounding) for example is impartial to party size. On
the other hand, the *Huntington–Hill* method favors small parties.
Generally, Huntington-Hill is used with a quorum, otherwise all parties
with more than zero votes get at least one seat. In this example, the
quorum is 3% of votes in a district.

``` r

bydistrict_v1 = apply_proporz(votes_matrix, district_seats,
                              method = "sainte-lague")

bydistrict_v2 = apply_proporz(votes_matrix, district_seats,
                              method = "huntington-hill", 
                              quorum = 0.03)
```

Let’s compare the seat distributions for these three methods. We get the
number of party seats on the national level with `rowSums`.

``` r

df_bydistrict = data.frame(
  D.Hondt = rowSums(bydistrict_v0),
  Sainte.Lague = rowSums(bydistrict_v1),
  Huntington.Hill = rowSums(bydistrict_v2)
)

# sort table by D'Hondt seats
df_bydistrict <- df_bydistrict[order(df_bydistrict[[1]], decreasing = TRUE),]

# print parties with at least one seat
knitr::kable(df_bydistrict[rowSums(df_bydistrict) > 0,])
```

|      | D.Hondt | Sainte.Lague | Huntington.Hill |
|:-----|--------:|-------------:|----------------:|
| SDP  |      40 |           36 |              36 |
| PS   |      39 |           37 |              37 |
| KOK  |      38 |           35 |              35 |
| KESK |      31 |           29 |              29 |
| VIHR |      20 |           25 |              27 |
| VAS  |      16 |           18 |              18 |
| RKP  |       9 |            8 |               8 |
| KD   |       5 |            7 |               6 |
| Nyt  |       1 |            2 |               2 |
| NYT  |       0 |            1 |               1 |
| SIN  |       0 |            1 |               0 |

The actual political analysis is better left for people familiar with
the party system. However, it is fair to say that the election system
within districts has a significant impact on the number of seats in
parliament. For example, the party with the most seats changes with
Sainte-Laguë or Huntington-Hill.

### 3.4) Compare seat share with vote share

Let’s now compare the vote shares on the national level with the seat
share in parliament. Since every entry in `votes_matrix` is only one
voter, we can simply use the row sums to get the vote shares. Otherwise,
we’d have to weigh votes in each district according to the number of
seats. Since disproportionality analysis is not the focus of this
package, we’ll simply look at the difference in shares for the actual
distribution method.

``` r

vote_shares = rowSums(votes_matrix)/sum(votes_matrix)

shares = data.frame(
  seats = rowSums(bydistrict_v0)/sum(district_seats),
  votes = vote_shares 
)
shares$difference <- shares$seats-shares$votes
shares <- round(shares, 4)

# Only look at parties with at least 0.5 % of votes
shares <- shares[shares$votes > 0.005,]
shares <- shares[order(shares$difference),]

shares
#>       seats  votes difference
#> VIHR 0.1005 0.1154    -0.0149
#> KD   0.0251 0.0391    -0.0140
#> SIN  0.0000 0.0098    -0.0098
#> PIR  0.0000 0.0062    -0.0062
#> Nyt  0.0050 0.0081    -0.0030
#> VAS  0.0804 0.0821    -0.0017
#> RKP  0.0452 0.0455    -0.0003
#> KESK 0.1558 0.1381     0.0176
#> KOK  0.1910 0.1707     0.0202
#> PS   0.1960 0.1756     0.0204
#> SDP  0.2010 0.1781     0.0229
```

The difference between seat and vote share varies among parties from
-1.5 to +2.3 percentage points.

  

## 4) Biproportional method

### 4.1) Biproportional party seats

We’ll now look at biproportional apportionment and whether it better
matches the seat share with the national vote share. Keep in mind that
simply using existing data sets with `biproporz` is not really suitable
from a modeling perspective. Vote distributions are likely to be
different if, for example, people know their vote also counts on a
national level or if smaller parties choose to run in more districts.

With that being said, let’s assume our data set is properly modeled
already. We need to consider that voters in Finland can only vote for
*one* person in each district. They can’t distribute as many votes as
there are seats in their district among candidates/parties. Thus we need
to use `weight_votes = FALSE` as a parameter in `biproporz`.

``` r

seats_biproportional = biproporz(votes_matrix,
                                 district_seats,
                                 weight_votes = FALSE)

# show only parties with seats
seats_biproportional[rowSums(seats_biproportional) > 0,]
```

|      | HÄM | HEL | KAA | KES | LAP | OUL | PIR | SAT | SKA | UUS | VAA | VAR |
|:-----|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
| KD   |   1 |   0 |   1 |   1 |   0 |   1 |   1 |   0 |   1 |   1 |   1 |   0 |
| KESK |   1 |   1 |   3 |   2 |   2 |   6 |   2 |   1 |   3 |   2 |   3 |   2 |
| KOK  |   3 |   5 |   3 |   1 |   1 |   2 |   4 |   1 |   2 |   7 |   2 |   4 |
| KP   |   0 |   0 |   0 |   0 |   0 |   0 |   1 |   0 |   0 |   0 |   0 |   0 |
| Nyt  |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   2 |   0 |   0 |
| NYT  |   0 |   1 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |
| PIR  |   0 |   1 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |
| PS   |   3 |   3 |   3 |   2 |   1 |   4 |   3 |   2 |   3 |   5 |   3 |   3 |
| RKP  |   0 |   1 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   4 |   3 |   1 |
| SDP  |   4 |   3 |   4 |   2 |   1 |   2 |   4 |   2 |   3 |   6 |   2 |   3 |
| SIN  |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   1 |   1 |   0 |   0 |
| STL  |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   1 |   0 |   0 |
| VAS  |   1 |   2 |   1 |   1 |   1 |   2 |   2 |   1 |   1 |   2 |   1 |   2 |
| VIHR |   1 |   5 |   2 |   1 |   1 |   1 |   2 |   1 |   1 |   5 |   1 |   2 |

Let’s now look at the difference between vote and seat share.

``` r

vote_shares = rowSums(votes_matrix)/sum(votes_matrix)
seat_shares = rowSums(seats_biproportional)/sum(seats_biproportional)

range(vote_shares - seat_shares)
#> [1] -0.005144852  0.002429257
```

As we can see, the difference between vote and seat share ranges from
-0.5 to +0.2 percentage point. Biproportional apportionment matches the
national vote share better than apportionment by district. This is
expected however, since biproportional apportionment actually considers
the national vote share. Discussing the pros and cons of a regional
representation compared to a priority on national vote shares is not
within the scope of this vignette. The following chunk shows the seat
changes.

``` r

seat_changes = seats_biproportional - bydistrict_v0

.parties_w_changes = rowSums(abs(seat_changes)) > 0
.districts_w_changes = colSums(abs(seat_changes)) > 0
knitr::kable(seat_changes[.parties_w_changes, .districts_w_changes])
```

|      | HEL | KAA | KES | LAP | OUL | PIR | SAT | SKA | UUS | VAA | VAR |
|:-----|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|----:|
| KD   |   0 |   1 |   1 |   0 |   1 |   0 |   0 |   0 |   0 |   0 |   0 |
| KESK |   1 |   0 |  -1 |  -1 |   0 |   0 |   0 |  -1 |   0 |  -1 |   0 |
| KOK  |  -1 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |  -2 |   0 |   0 |
| KP   |   0 |   0 |   0 |   0 |   0 |   1 |   0 |   0 |   0 |   0 |   0 |
| Nyt  |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   1 |   0 |   0 |
| NYT  |   1 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |
| PIR  |   1 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |
| PS   |   0 |  -1 |   0 |   0 |   0 |  -1 |   0 |   0 |  -1 |   0 |  -1 |
| RKP  |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   1 |  -1 |   0 |
| SDP  |   0 |  -1 |   0 |   0 |   0 |  -1 |  -1 |   0 |  -1 |   0 |   0 |
| SIN  |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   1 |   1 |   0 |   0 |
| STL  |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   0 |   1 |   0 |   0 |
| VAS  |  -1 |   1 |   0 |   0 |  -1 |   1 |   0 |   0 |   0 |   1 |   0 |
| VIHR |  -1 |   0 |   0 |   1 |   0 |   0 |   1 |   0 |   0 |   1 |   1 |

### 4.2) Distribute seats among districts as well

Normally the number of seats per district is defined before an election,
based on census data. However, you *could* also assign the number of
seats per district, based on the actual votes cast in every district.
This way, each districts seat share is proportional to its vote share.
To do this, we use the total number of seats for the `district_seats`
parameter.

``` r

full_biproportional = biproporz(votes_matrix,
                                district_seats = sum(district_seats),
                                weight_votes = FALSE)

# party seat distribution has not changed
rowSums(full_biproportional) - rowSums(seats_biproportional)
#>   Asyl   E117   E118   E154   E168   E169   E185   E191   E192   E287   E491 
#>      0      0      0      0      0      0      0      0      0      0      0 
#>   E492   E493    EOP     FP     IP     KD   KESK    KOK     KP    KTP   LIBE 
#>      0      0      0      0      0      0      0      0      0      0      0 
#>    LII   LIIK     LN   LNLY    LNY   LNYL    Nyt    NYT    PEL    PIR     PS 
#>      0      0      0      0      0      0      0      0      0      0      0 
#>    REF Reform    RKP    RLI    SDP    SIN    SKE    SKP    STL    VAS   VIHR 
#>      0      0      0      0      0      0      0      0      0      0      0

# district seat distribution is different
colSums(full_biproportional) - colSums(seats_biproportional)
#> HÄM HEL KAA KES LAP OUL PIR SAT SKA UUS VAA VAR 
#>  -1   3  -1   0   0  -1   0   0  -1   0   0   1
```

As we can see, the number of party seats does not change. However,
districts where more people voted get a higher share of the seats. 2
districts gain seats, 4 districts lose a seat.

  

## 5) Effect of a system change on seat distribution

As a last example, we’ll look at the changes in seat distribution
biproportional apportionment had in the 2020 parliament election in the
Swiss canton of Uri. In Uri, 2020 was the first year the cantonal
parliament was elected with biproportional apportionment for some
municipalities. Previously, these four municipalities used the
*Hagenbach-Bischoff* method. We’ll ignore all other municipalities which
use a majoritarian electoral system.

``` r

seats_old_system = apply_proporz(uri2020$votes_matrix,
                                 uri2020$seats_vector,
                                 "hagenbach-bischoff")

seats_new_system = biproporz(uri2020$votes_matrix, uri2020$seats_vector)

addmargins(seats_new_system - seats_old_system)
#>      Altdorf Bürglen Erstfeld Schattdorf Sum
#> CVP        1       0        0          0   1
#> SPGB      -1       0        0          0  -1
#> FDP        0       0        0          0   0
#> SVP        0       0        0          0   0
#> Sum        0       0        0          0   0
```

Compared to the previous election system there was a change of one seat
(out of 37).

``` r
library(proporz)

# data set
votes_matrix = pivot_to_matrix(finland2019$votes_df)
party_votes = rowSums(votes_matrix)
district_seats = finland2019$district_seats_df$election_mandates
names(district_seats) <- finland2019$district_seats_df$entity_id

# calculate the seat distribution in each district with a given method
# returns the national seat distribution
seats_by_district = function(votes_matrix, district_seats, method, quorum = 0) {
	x = votes_matrix
	x[] <- NA
	for(district in names(district_seats)) {
		x[,district] <- proporz(votes_matrix[,district],
								district_seats[district],
								quorum = quorum,
								method = method)
	}
	rowSums(x)
}

# This is how the Finnish election system currently works
d.hondt = seats_by_district(votes_matrix, district_seats, "d'hondt")

# It's also possible to distribute seats within a district according to a different
# method. Sainte-Laguë (standard rounding) is impartial to party size
sainte.lague = seats_by_district(votes_matrix, district_seats, "sainte-lague")

# you could also you the Huntington–Hill method which favors small parties
huntington.hill = seats_by_district(votes_matrix, district_seats, "huntington-hill", quorum = 0.005)

# With the Pukelsheim method, the upper apportionment calculates the party seat
# distribution on the national level first before apportioning party seats among district
UA_d.hondt = proporz(rowSums(votes_matrix), sum(district_seats), "d'hondt")
# same as upper_apportionment(votes_matrix, district_seats, use_list_votes = FALSE)

# Again, it's possible to use a different method for the upper apportionment, like
# the standard sainte-lague
UA_sainte.lague = proporz(rowSums(votes_matrix), sum(district_seats), "sainte-lague")

# Create a table to compare the parlament seat distribution (ignoring which party seats
# are assigned to which district)
df = data.frame(
	d.hondt,
	sainte.lague,
	huntington.hill,
	UA_d.hondt,
	UA_sainte.lague)
df <- df[order(df[[1]], decreasing = T),] # sort table

# print parties with at least one seat
df[which(rowSums(apply(df, 2, function(x) x > 0)) > 0),]
#>      d.hondt sainte.lague huntington.hill UA_d.hondt UA_sainte.lague
#> SDP       40           36              32         37              36
#> PS        39           37              30         36              35
#> KOK       38           35              32         35              35
#> KESK      31           29              22         28              28
#> VIHR      20           25              20         24              23
#> VAS       16           18              16         17              17
#> RKP        9            8               8          9               9
#> KD         5            7              11          8               8
#> Nyt        1            2               2          1               2
#> E185       0            0               1          0               0
#> FP         0            0               1          0               0
#> KP         0            0               2          0               1
#> LII        0            0               1          0               0
#> LIIK       0            0               1          0               0
#> LN         0            0               2          0               0
#> LNLY       0            0               1          0               0
#> LNY        0            0               1          0               0
#> LNYL       0            0               1          0               0
#> NYT        0            1               2          1               1
#> PIR        0            0               4          1               1
#> SIN        0            1               8          2               2
#> STL        0            0               1          0               1

# With methods that favor small parties like Huntington-Hill or Adams no quorum leads to
# a lot of small parties getting a seat
sort(seats_by_district(votes_matrix, district_seats, "adams", quorum = 0))
#>   E117   E154   E168   E191   E192    PEL   Asyl   E118   E169   E185   E287
#>      0      0      0      0      0      0      1      1      1      1      1
#>   E491   E492   E493    LII   LIIK   LNLY    LNY   LNYL    Nyt    REF Reform
#>      1      1      1      1      1      1      1      1      1      1      1
#>    RLI     LN    NYT    KTP    EOP   LIBE    SKE    SKP     FP     IP     KP
#>      1      2      2      3      4      6      6      6      7      7      7
#>    RKP    STL    PIR    SIN     KD    VAS   KESK     PS    KOK    SDP   VIHR
#>      8      9     10     10     11     12     13     14     15     15     15
```

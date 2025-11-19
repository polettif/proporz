# proporz (dev)

* assert vector-matrix name matching in `lower_apportionment()`

# proporz 1.5.2

* weight_list_votes() is deprecated, renamed  to `weight_votes_matrix()`
* Parameter `use_list_votes` in `biproporz()`/`pukelsheim()` is deprecated, renamed to `weight_votes`
* `proporz::proporz()` can now be used without calling `library(proporz)`
* `pukelsheim()` returns tibbles for tibble input

# proporz 1.5.1

* new "Modifying biproporz()" vignette (#15)
* add `district_winner_matrix()` function, exported internal implementation (#13)
* add `apply_quorum()` function, exported internal implementation
* add `summary()` method for biproporz results which shows marginal sums and divisors
* returned seat values from proporz/biproporz functions are always integer
* undefined biproporz results are caught earlier, added flow criterion check (#14)
* minor changes to error messages

# proporz 1.5.0

* add new feature: winner take one method for `biproporz()` and `pukelsheim()` (#10)
* allow non-integer vote counts
* round divisors to as few digits as possible
* changed error message for undefined/tied results
* fixed bugs and added more real data tests

# proporz 1.4.0

* first CRAN release
* add vignette and examples
* removed `biproportional()`, only use `biproporz()`
* renamed `quota_largest_remainder()` to `largest_remainder_method()`
* renamed `divisors()` to `get_divisors()`
* removed `hzv()` alias and made `reached_quorums()` internal
* update documentation and tests
* minor bugfixes

# proporz 1.3.1

* error if not enough seats are available for harmonic/geometric divisor methods
* always error on equal remainders

# proporz 1.3.0

* rework and expand quorum calculations (#4)
* deprecated quorum_total and quorum_districts parameters

# proporz 1.2.1

* allow number of seats to be 0 instead of throwing error

# proporz 1.2.0

* throw error if no solution can be found
* added basic shiny app

# proporz 1.1.0

* apportion method and use_list_votes parameter for biproportional() 
* add finland2019 dataset
* export lower and upper apportionment functions
* improve divisor search

# proporz 1.0.0

* Publish package to Github

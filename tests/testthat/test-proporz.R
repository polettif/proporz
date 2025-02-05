test_that("generic proporz", {
    expect_identical(
        proporz(c(216, 310, 32), 20, "jefferson"),
        proporz(c(216, 310, 32), 20, "D'hondt"))
    expect_identical(
        proporz(c(216, 310, 32), 20, "Hagenbach-Bischoff"),
        proporz(c(216, 310, 32), 20, "hare-niemeyer"))
    expect_error(proporz(1,1, "unkown method"), "Unknown apportion method")
    v = c(10, 690, 210, 370)
    expect_identical(proporz(v*.02, 10, "round"), proporz(v*0.1, 10, "round"))
})

.sample_votes = function(n_non_zero, n_zero) {
    repeat {
        x = round(stats::runif(n_non_zero, 10, 1000))
        if(length(unique(x)) == n_non_zero) {
            break
        }
    }
    return(c(x, rep(0, n_zero)))
}

test_that("proporz parameter range", {
    method_list = unique(unlist(proporz_methods, use.names = FALSE))

    set.seed(0)
    for(n_parties in 1:2) {
        for(n_parties_zero in 0:2) {
            for(n_seats in 0:6) {
                for(method_impl in method_list) {
                    votes = .sample_votes(n_parties, n_parties_zero)

                    if(n_seats < n_parties &&
                       method_impl %in% c("divisor_ceiling", "divisor_geometric", "divisor_harmonic") &&
                       n_seats > 0) {
                        .method_impl = gsub("divisor_", "", method_impl, fixed = TRUE)
                        expect_error(
                            proporz(votes, n_seats, method_impl),
                            paste0("With ",  .method_impl, " rounding there must be at ",
                                   "least as many seats as there are parties with non-zero votes."),
                            fixed = TRUE)
                    } else {
                        seats = proporz(votes, n_seats, method_impl)
                        assert(is.integer(seats))
                        expect_true(is.integer(seats))
                        expect_identical(length(seats), length(votes))
                        expect_identical(sum(seats), n_seats)

                        if(n_seats > 0) {
                            .quorum = sort(c(votes,0), decreasing = TRUE)[2]+0.5
                            seats_Q = proporz(votes, n_seats, method_impl, quorum = .quorum)
                            expect_identical(sum(seats_Q > 0), 1L)
                        }
                    }
                }
            }
        }
    }

    # unsupported values
    for(method_impl in method_list) {
        for(n_seats in list(NA, NULL, -1, 1.1, c(1, 1))) {
            expect_error(proporz(c(100, 10, 5), n_seats, method_impl), "`n_seats` must be an integer >= 0")
        }
    }
    for(method_impl in method_list) {
        for(votes in list(NA, NULL, -1)) {
            expect_error(proporz(votes, 3, method_impl), "`votes` must be a numeric vector >= 0", fixed = TRUE)
        }
    }
})

test_that("quorum", {
    method_list = unique(unlist(proporz_methods, use.names = FALSE))

    for(method in method_list) {
        expect_error(proporz(c(50, 30), 3, method, 60), "No party reached the quorum.",
                     fixed = TRUE)
    }
})

test_that("all method names", {
    for(m in names(proporz_methods)) {
        x = proporz(c(10, 20, 5), 3, m)
        expect_length(x, 3)
    }
})

test_that("undefined result errors", {
    expect_error(proporz(c(1, 10, 10), 1, "round"),
                 "Result is undefined, equal quotient for parties: 2, 3", fixed = TRUE)
    expect_error(proporz(c(1, X = 10, ABC = 10), 1, "round"),
                 "Result is undefined, equal quotient for parties: 'X', 'ABC'", fixed = TRUE)
    expect_identical(proporz(c(1, 10, 10), 2, "round"), c(0L,1L,1L))

    expect_error(largest_remainder_method(c(10, 10, 0), 1),
                 "Result is undefined, equal remainder for parties: 1, 2",
                 fixed = TRUE)
    expect_error(largest_remainder_method(c(A = 10, X = 10, P = 0), 1),
                 "Result is undefined, equal remainder for parties: 'A', 'X'",
                 fixed = TRUE)
})

test_that("huntington-hill", {
    # https://www.census.gov/data/tables/2020/dec/2020-apportionment-data.html
    us_census_2020 = structure(list(
        state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
                  "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
        population = c(5030053, 736081, 7158923, 3013756, 39576757, 5782171, 3608298, 990837, 21570527, 10725274, 1460137, 1841377, 12822739, 6790280, 3192406, 2940865, 4509342, 4661468, 1363582, 6185278, 7033469, 10084442, 5709752, 2963914, 6160281, 1085407,
                       1963333, 3108462, 1379089, 9294493, 2120220, 20215751, 10453948, 779702, 11808848, 3963516, 4241500, 13011844, 1098163, 5124712, 887770, 6916897, 29183290, 3275252, 643503, 8654542, 7715946, 1795045, 5897473, 577719),
        apportioned_representatives = c(7L, 1L, 9L, 4L, 52L, 8L, 5L, 1L, 28L, 14L, 2L, 2L, 17L, 9L, 4L, 4L, 6L, 6L, 2L, 8L, 9L, 13L, 8L, 4L, 8L, 2L, 3L, 4L, 2L, 12L, 3L, 26L, 14L, 1L, 15L, 5L, 6L, 17L, 2L, 7L, 1L, 9L, 38L, 4L, 1L, 11L, 10L, 2L, 8L, 1L)),
        row.names = c(NA, -50L), class = "data.frame")

    x = proporz(us_census_2020$population, 435, "huntington-hill")
    expect_identical(x, us_census_2020$apportioned_representatives)
})

test_that("sainte-lague", {
    stimmen1 = c(11828277, 9990488, 6316080, 5155933, 4643272, 2830238)
    expect_identical(divisor_round(stimmen1, 598),
                     as.integer(c(173, 146, 93, 76, 68, 42)))

    stimmen2 = c(430739, 143607, 242942, 1297940, 102419, 348216, 202850, 348082,
                 2678956, 328753, 812721, 216593, 520990, 1120018, 1051198, 144464)
    expect_identical(divisor_round(stimmen2, 146),
                     as.integer(c(6,2,4,19,2,5,3,5,39,5,12,3,8,16,15,2)))
})

test_that("huntington-hill and adams give at least one seat", {
    v = c(100, 1, 0, 10, 0)
    expect_identical(divisor_round(v, 3), c(3L,0L,0L,0L,0L))
    dc = divisor_ceiling(v, 3)
    expect_identical(dc, c(1L,1L,0L,1L,0L))
    expect_identical(proporz(v, 3, "huntington-hill"), dc)
    expect_identical(proporz(v, 3, "dean"), dc)
    expect_identical(proporz(v, 3, "adams"), dc)
})

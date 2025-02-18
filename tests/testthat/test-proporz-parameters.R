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
                                   "least as many seats as there are parties with non-zero votes"),
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

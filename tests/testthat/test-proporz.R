context("proporz")

test_that("generic proporz", {
	expect_equal(
		proporz(c(216, 310, 32), 20, "jefferson"),
		proporz(c(216, 310, 32), 20, "D'hondt"))
	expect_equal(
		proporz(c(216, 310, 32), 20, "Hagenbach-Bischoff"),
		proporz(c(216, 310, 32), 20, "hare-niemeyer"))
	expect_error(proporz(1,1, "unkown method"))
})

test_that("proporz parameter range", {
	method_list = unique(unlist(apport_methods, use.names = F))

	set.seed(0)
	for(n_parties in 1:2) {
		for(n_parties_zero in 0:2) {
			for(n_seats in 0:6) {
				for(method in method_list) {
					votes = .sample_votes(n_parties, n_parties_zero)
					seats = proporz(votes, n_seats, method)

					expect_equal(length(seats), length(votes))
					expect_equal(sum(seats), n_seats)
				}
			}
		}
	}

	# unsupported values
	for(method in method_list) {
		for(n_seats in list(NA, NULL, -1, c(1, 1))) {
			expect_error(proporz(c(100, 10, 5), n_seats, method), "n_seats must be one number >= 0")
		}
	}
	for(method in method_list) {
		for(votes in list(NA, NULL, -1)) {
			expect_error(proporz(votes, 3, method), "votes must be numeric >= 0", fixed = TRUE)
		}
	}
})

test_that("all method names", {
	for(m in names(apport_methods)) {
		x = proporz(c(10, 20, 5), 3, m)
		expect_length(x, 3)
	}
})

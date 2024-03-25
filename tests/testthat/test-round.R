test_that("ceil_at", {
    expect_error(ceil_at(-10.5, 0.5))
    expect_equal(round(0.5), 0)
    expect_equal(ceil_at(0.5, 0.5), 1)
    expect_equal(ceil_at(1.5, 0.5), 2)
    expect_equal(ceil_at(2.5, 0.5), 3)
    expect_equal(ceil_at(0.1, 0.1), 1)

    vec1 = seq(2.05, 2.2, by = 0.05)
    expect_equal(ceil_at(vec1, 0.15), c(2,2,3,3))
    vec2 = seq(2.2, 2.05, by = -0.05)
    expect_equal(ceil_at(vec2, 0.15), c(3,3,2,2))

    M = matrix(c(0,1.5,2.5,3.5), ncol = 2)
    expect_equal(ceil_at(M, 0.5), round(M+0.001))
    expect_equal(ceil_at(M, 0), ceiling(M))
    expect_equal(ceil_at(M, 1), floor(M))

    expect_error(proporz(1,1, "unkown method"))
    expect_error(ceil_at(1.6, NA), "is.na")
    expect_error(ceil_at(1.6, -1), "Threshold argument must be in [0,1]", fixed = TRUE)
    expect_error(ceil_at(0.5, "x"), 'Numeric value, "harmonic" or "geometric" expected for threshold argument')
    expect_error(ceil_at(-1, 0))
    expect_error(ceil_at(NA, 0))
})

test_that("ceil_at harmonic/geometric", {
    # harmonic
    # https://de.wikipedia.org/wiki/Dean-Verfahren
    wiki = function(n) {
        (n-1) + ((n-1) / (2*n-1))
    }
    expect_equal(
        threshold_harmonic(c(9-1e-12, 9+1e-12,  12+1e-12, 13-1e-12)),
        wiki(c(9, 10, 13, 13)))
    expect_equal(threshold_harmonic(c(9,12)), c(9,12))
    expect_equal(
        ceil_at(c(2-1e-12, 2, 2+1e-12, 7-1e-12, 7, 7+1e-12), "harmonic"),
        c(2,2,2,7,7,7))
    expect_equal(ceil_at(c(wiki(3), wiki(90)), "harmonic"), c(3, 90))
    expect_equal(ceil_at(c(wiki(3), wiki(90))-1e-12, "harmonic"), c(3, 90)-1)

    # geometric
    expect_equal(
        threshold_geometric(c(2-1e-12, 2, 2+1e-12, 7-1e-12, 7, 7+1e-12)),
        c(sqrt(2), 2, sqrt(3*2), sqrt(7*6), 7, sqrt(8*7)),
        tolerance = 1e-14)
    expect_equal(
        ceil_at(c(2-1e-12, 2, 2+1e-12, 7-1e-12, 7, 7+1e-12), "geometric"),
        c(2,2,2,7,7,7))
    expect_equal(ceil_at(c(sqrt(2*3), sqrt(89*90)), "geometric"), c(3, 90))
    expect_equal(ceil_at(c(sqrt(2*3), sqrt(89*90))-1e-12, "geometric"), c(3, 90)-1)

    # edge cases
    expect_equal(threshold_geometric(0), 0)
    expect_equal(threshold_harmonic(0), 0)
    expect_equal(ceil_at(0, "harmonic"), 0)
    expect_equal(ceil_at(0, "geometric"), 0)
    expect_equal(ceil_at(0, 0), 0)
    expect_equal(ceil_at(0+1e-12, "harmonic"), 1)
    expect_equal(ceil_at(0+1e-12, "geometric"), 1)
    expect_equal(ceil_at(0+1e-12, 0), 1) # ceiling
})

test_that("ceil_at harmonic/geometric matrix", {
    x = c(0,1,42,99)
    x_non0 = x[2:4]
    eps = 0.000001
    M = matrix(c(x,
                 0.000, 1.333333+eps, 42.494118+eps, 99.497487+eps, # harmonic
                 0.000, 1.414214+eps, 42.497059+eps, 99.498744+eps, # geometric
                 0.500, 1.500000, 42.500000, 99.500000, # round
                 x+1), nrow = 5, byrow = TRUE)

    R_0.5 = matrix(c(x,x,x,x+1,x+1), nrow = 5, byrow = TRUE)
    expect_equal(ceil_at(M, 0.5), R_0.5)

    R_geometric = matrix(c(x, x, c(0,x_non0+1), x+1, x+1), nrow = 5, byrow = TRUE)
    expect_equal(ceil_at(M, "geometric"), R_geometric)

    R_harmonic = matrix(c(x, c(0,x_non0+1), c(0,x_non0+1), x+1, x+1), nrow = 5, byrow = TRUE)
    expect_equal(ceil_at(M, "harmonic"), R_harmonic)

    expect_equal(threshold_geometric(18.4), threshold_geometric(18.5))
    set.seed(1)
    i = runif(n = 100, 0, 200)
    expect_true(all(threshold_harmonic(i) < threshold_geometric(i)))
})

test_that("ceil_at 0.5_at_least_one", {
    x = c(0.1, 0.5, 1, 1.4, 1.5, 9.5, 10.5)
    expect_equal(ceil_at(x, "0.5_at_least_one"), c(1,1,1, 1,2,10,11))
    m = matrix(c(0.1, 0.5, 1, 1.4, 1.5, 9.5, 10.5, 9), ncol = 2)
    m_exp = matrix(c(1,1,1, 1,2,10,11,9), ncol = 2)
    expect_equal(ceil_at(m, "0.5_at_least_one"), m_exp)
})

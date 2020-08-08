context("round")

test_that("ceil_at", {
    expect_error(ceil_at(-10.5, 0.5))
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
})

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

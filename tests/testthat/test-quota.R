context("quota")

# https://de.wikipedia.org/wiki/Hare-Niemeyer-Verfahren
test_that("quota_largest_remainder", {
    v1 = c(216, 310, 22, 32)
    n1 = 60
    e1 = c(23,32,2,3)
    expect_equal(quota_largest_remainder(v1, n1), e1)
    expect_equal(proporz(v1, n1, "hare-niemeyer"), e1)
    expect_equal(proporz(v1, n1, "hamilton"), e1)
    expect_equal(proporz(v1, n1, "vinton"), e1)
})

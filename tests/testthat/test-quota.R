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

    # https://en.wikipedia.org/wiki/Largest_remainder_method
    v2 = c(47000, 16000, 15800, 12000, 6100, 3100)
    e2 = c(5, 2, 1, 1, 1, 0)
    expect_equal(quota_largest_remainder(v2, 10), e2)

    # Wikipedia DE
    v3 = c(720257, 323524, 257466, 213138, 144392, 88315)
    e3 = c(13, 6, 5, 4, 2, 1)
    expect_equal(quota_largest_remainder(v3, 31), e3)

    expect_error(quota_largest_remainder(numeric(), numeric()))
})

test_that("equal remainder not on threshold", {
    votes = c(43, 33, 12, 8, 4)
    expect_equal(quota_largest_remainder(votes, 10), c(4,3,1,1,1))
    expect_equal(
        quota_largest_remainder(votes, 10, 5), c(5,3,1,1,0))
    expect_equal(
        quota_largest_remainder(votes, 10, quorum = 0.045), c(5,3,1,1,0))

    # no remainders
    expect_equal(quota_largest_remainder(c(10, 10, 0), 2), c(1,1,0))
})

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
    votes2 = c(47000, 16000, 15800, 12000, 6100, 3100)
    seats_actual2 = quota_largest_remainder(votes2, 10)
    seats_expected2 = c(5, 2, 1, 1, 1, 0)
    expect_equal(seats_actual2, seats_expected2)

    # Wikipedia DE
    votes3 = c(720257, 323524, 257466, 213138, 144392, 88315)
    seats_expected3 = c(13, 6, 5, 4, 2, 1)
    seats_actual3 = quota_largest_remainder(votes3, 31)
    expect_equal(seats_actual3, seats_expected3)

    expect_error(quota_largest_remainder(numeric(), numeric()))
})

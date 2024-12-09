# https://de.wikipedia.org/wiki/Hare-Niemeyer-Verfahren
test_that("largest_remainder_method", {
    v1 = c(216, 310, 22, 32)
    n1 = 60
    e1 = c(23L,32L,2L,3L)
    expect_identical(largest_remainder_method(v1, n1), e1)
    expect_identical(proporz(v1, n1, "hare-niemeyer"), e1)
    expect_identical(proporz(v1, n1, "hamilton"), e1)
    expect_identical(proporz(v1, n1, "vinton"), e1)

    # https://en.wikipedia.org/wiki/Largest_remainder_method
    v2 = c(47000, 16000, 15800, 12000, 6100, 3100)
    e2 = c(5L, 2L, 1L, 1L, 1L, 0L)
    expect_identical(largest_remainder_method(v2, 10), e2)

    # Wikipedia DE
    v3 = c(720257, 323524, 257466, 213138, 144392, 88315)
    e3 = c(13L, 6L, 5L, 4L, 2L, 1L)
    expect_identical(largest_remainder_method(v3, 31), e3)

    expect_error(largest_remainder_method(numeric(), numeric()))
    expect_error(largest_remainder_method(c(10,20), 2.2), "`2.2` must be an integer >= 0")

    # lr
    expect_error(lr_quota(c(100,200), 2, "test"), "Unknown quota method 'test'")

    # names
    v4 = v1
    names(v4) <- c("A", "B", "C", "D")
    expect_identical(names(largest_remainder_method(v4, 10)), names(v4))
})

test_that("equal remainder not on threshold", {
    votes = c(43, 33, 12, 8, 4)
    expect_identical(largest_remainder_method(votes, 10), as.integer(c(4,3,1,1,1)))
    expect_identical(
        largest_remainder_method(votes, 10, 5), as.integer(c(5,3,1,1,0)))
    expect_identical(
        largest_remainder_method(votes, 10, quorum = 0.045), as.integer(c(5,3,1,1,0)))

    # no remainders
    expect_identical(largest_remainder_method(c(10, 10, 0), 2), as.integer(c(1,1,0)))
})

test_that("read_bazi_data", {
    data1 = read_bazi_data(test_path("data/test1.bazi"))
    expect_identical(dim(data1$data), c(12L, 3L))

    data2a = read_bazi_data(test_path("data/test2a.bazi"))
    expect_identical(dim(data2a$data), c(3L, 3L))
    expect_identical(data2a$data[["votes"]], c(651L, 349L, 50L))

    data2b = read_bazi_data(test_path("data/test2b.bazi"))
    expect_identical(dim(data2b$data), c(3L, 2L))
    expect_identical(data2b$data[["votes"]], c(651L, 349L, 50L))

    data3 = read_bazi_data(test_path("data/test3.bazi"))
    expect_identical(colnames(data3$data),
                     c("partyname", "DISTRICT", "votes__", "actualseats"))

    x3 = data3$data[!is.na(data3$data$votes__), ]
    bp3 = biproporz(pivot_to_matrix(x3[, c(1, 2, 3)]), 18)
    expect_identical(dimnames(bp3),
                     list(partyname = c("ABC", "DEF", "XYZ"),
                          DISTRICT = c("g", "h", "i", "j")))
})

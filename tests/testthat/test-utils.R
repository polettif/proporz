context("utils")

test_that("hoechstzahlverfahren", {
    expect_equal(hzv(c(5200, 1700, 3100), 15, 0.5), c(8,2,5))
    expect_equal(hzv(c(100, 80, 30, 20), 8, 1), c(4,3,1,0))
})

test_that("bisect", {
    funk = function(x) x^3 - x - 2
    act = bisect(funk, 1, 2, 1e-7)
    exp = 1.5214
    expect_equal(round(act, 4), exp)
})

test_that("quorum", {
    votes = c(49, 38, 13)
    expect_equal(apply_quorum_vector(votes, 0), c(49, 38, 13))
    expect_equal(apply_quorum_vector(votes, 0.13), c(49, 38, 13))
    expect_equal(apply_quorum_vector(votes, 0.135), c(49, 38, 0))
    expect_equal(apply_quorum_vector(votes, 15), c(49, 38, 0))
})

test_that("stupid pivot functions", {
    df0 = data.frame(id = c("a", "a", "a", "a", "b", "b"),
                     key = c("w", "x", "y", "z", "x", "y"),
                     value = 1:6, stringsAsFactors = F)

    matrix1 = pivot_to_matrix(df0)
    expect_equal(pivot_to_matrix(df0[c(2,1,3)]),
                 t(pivot_to_matrix(df0)))
    expect_true(all(!is.na(matrix1)))
    expect_is(matrix1, "matrix")
    expect_equal(rownames(matrix1), unique(df0[,1]))
    expect_equal(colnames(matrix1), unique(df0[,2]))
    expect_equal(c(matrix1), c(1,0,2,5,3,6,4,0))

    df1 = pivot_to_df(matrix1, "v")
    expect_equivalent(df1[which(df1$v > 0),], df0)

    expect_equal(colnames(pivot_to_df(unname(matrix1), "val")),
                 c("row", "col", "val"))
})

test_that("print", {
    M = biproporz(matrix(c(51,60,63,98,100,102,45,120,144), nrow = 3), 4:6)
    expect_equal(capture.output(print(M)), capture.output(print(as.matrix(M))))
})

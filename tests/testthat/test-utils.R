test_that("hoechstzahlverfahren", {
    expect_identical(highest_averages_method(c(5200, 1700, 3100), 15, 0.5), c(8L,2L,5L))
    expect_identical(highest_averages_method(c(100, 80, 30, 20), 8, 1), c(4L,3L,1L,0L))

    expect_error(highest_averages_method(c(100, 80, 30, 20), 3, c(0.5, 1.5)),
                 "Number of divisors is not equal to the number of seats")
})

test_that("bisect", {
    funk = function(x) x^3 - x - 2
    act = bisect(funk, 1, 2, 1e-7)
    exp = 1.5214
    expect_equal(round(act, 4), exp, tolerance = 1e-14)
})

test_that("quorum", {
    votes = c(49, 38, 13)
    expect_equal(apply_quorum_vector(votes, 0), c(49, 38, 13), tolerance = 1e-14)
    expect_equal(apply_quorum_vector(votes, 0.13), c(49, 38, 13), tolerance = 1e-14)
    expect_equal(apply_quorum_vector(votes, 0.135), c(49, 38, 0), tolerance = 1e-14)
    expect_equal(apply_quorum_vector(votes, 15), c(49, 38, 0), tolerance = 1e-14)
})

test_that("stupid pivot functions", {
    df0 = data.frame(id = c("a", "a", "a", "a", "b", "b"),
                     key = c("w", "x", "y", "z", "x", "y"),
                     value = 1:6, stringsAsFactors = FALSE)

    matrix1 = pivot_to_matrix(df0)
    expect_identical(pivot_to_matrix(df0[c(2,1,3)]),
                     t(pivot_to_matrix(df0)))
    expect_true(all(!is.na(matrix1)))
    expect_is(matrix1, "matrix")
    expect_identical(rownames(matrix1), unique(df0[,1]))
    expect_identical(colnames(matrix1), unique(df0[,2]))
    expect_identical(c(matrix1), as.integer(c(1,0,2,5,3,6,4,0)))

    df0i = df0
    df0i$value <- as.numeric(df0i$value)
    expect_true(is.integer(pivot_to_matrix(df0)))
    expect_false(is.integer(pivot_to_matrix(df0i)))

    df1 = pivot_to_df(matrix1, "v")
    expect_equivalent(df1[which(df1$v > 0),], df0)

    expect_identical(colnames(pivot_to_df(unname(matrix1), "val")),
                     c("row", "col", "val"))

    matrix2 = matrix(1:16, 4, 4)
    expect_identical(colnames(pivot_to_df(matrix2)), c("row", "col", "values"))
    matrix3 = matrix2
    colnames(matrix3) <- as.character(1:4)
    rownames(matrix3) <- c("A", "B", "C", "D")
    expect_identical(colnames(pivot_to_df(matrix3)), c("row", "col", "values"))
    matrix4 = matrix(as.numeric(1:16), 4, 4)
    expect_true(is.integer(pivot_to_df(matrix2)$values))
    expect_false(is.integer(pivot_to_df(matrix4)$values))
})

test_that("print", {
    M = biproporz(matrix(c(51,60,63,98,100,102,45,120,144), nrow = 3), 4:6)
    expect_identical(capture.output(print(M)), capture.output(print(as.matrix(M))))
})

test_that("collapse_names", {
    x = c("Abc", "XYZ")
    y = c(2, 6, 67)
    expect_identical(collapse_names(x), "'Abc', 'XYZ'")
    expect_identical(collapse_names(y), "2, 6, 67")
    expect_identical(collapse_names(c(T,F,T), c("A", "X", "B")), "'A', 'B'")
})

test_that("num_word", {
    expect_identical(num_word("party", "parties", 1), "party")
    expect_identical(num_word("party", "parties", 2), "party")
    expect_identical(num_word("party", "parties", c(2, 3)), "parties")
    expect_identical(num_word("party", "parties", c(T, F)), "party")
})

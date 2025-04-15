test_that("bisect", {
    funk = function(x) x^3 - x - 2
    act = bisect(funk, 1, 2, 1e-7)
    exp = 1.5214
    expect_equal(round(act, 4), exp, tolerance = 1e-14)
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

test_that("summary", {
    M = biproporz(uri2020$votes_matrix, uri2020$seats_vector)
    expect_identical(capture.output(summary(M)),c(
        "           Altdorf B\U{FC}rglen Erstfeld Schattdorf (sum) (divisor)",
        "       CVP       5       2        2          3    12     0.946",
        "      SPGB       4       1        2          2     9         1",
        "       FDP       3       1        1          2     7         1",
        "       SVP       3       3        1          2     9      0.97",
        "     (sum)      15       7        6          9    37          ",
        " (divisor)    2689    1194     1088       1539                "
    ))
    expect_identical(capture.output(summary(t(M))), c(
        "              CVP SPGB FDP  SVP (sum) (divisor)",
        "    Altdorf     5    4   3    3    15      2689",
        "    B\U{FC}rglen     2    1   1    3     7      1194",
        "   Erstfeld     2    2   1    1     6      1088",
        " Schattdorf     3    2   2    2     9      1539",
        "      (sum)    12    9   7    9    37          ",
        "  (divisor) 0.946    1   1 0.97                "
    ))
    M2 = M
    names(dimnames(M2)) <- c("Partei", "Gemeinde")
    expect_identical(summary(M2), summary(M))

    dimnames(M) <- NULL
    expect_error(summary(M), "proporz_matrix must have dimnames identical to divisor names")
    expect_error(summary(t(M)), "proporz_matrix must have dimnames identical to divisor names")
})

test_that("collapse_names", {
    x = c("Abc", "XYZ")
    y = c(2, 6, 67)
    expect_identical(collapse_names(x), "'Abc', 'XYZ'")
    expect_identical(collapse_names(y), "2, 6, 67")
    expect_identical(collapse_names(c(TRUE,FALSE,TRUE), c("A", "X", "B")), "'A', 'B'")
})

test_that("num_word", {
    expect_identical(num_word("party", "parties", 1), "party")
    expect_identical(num_word("party", "parties", 2), "party")
    expect_identical(num_word("party", "parties", c(2, 3)), "parties")
    expect_identical(num_word("party", "parties", c(TRUE, FALSE)), "party")
})

test_that("assert", {
    expect_error(assert(NA), "NA is not TRUE")
})

test_that("col/row max", {
    m = matrix(c(8,9,2,1), 2)
    expect_equal(row_maxs(m), c(8,9))
    expect_equal(col_maxs(m), c(9,2))
})

test_that("district_winner_matrix", {
    votes_matrix = matrix(c(90, 50, 60, 50, 10, 50), ncol = 3)
    dw = district_winner_matrix(votes_matrix)

    expect_equal(dw, matrix(c(T,F,T,F,F,T), ncol = 3))
    expect_true(is.logical(dw))
    expect_equal(colSums(dw), c(1,1,1))
    expect_equal(sum(colSums(dw)), 3)

    # ties
    votes_matrix[1,2] <- 50
    expect_error(district_winner_matrix(votes_matrix), "Tied majority in column 2")
    expect_error(district_winner_matrix(votes_matrix, ties = "error"), "Tied majority in column 2")
    expect_equal(colSums(district_winner_matrix(votes_matrix, ties = "random")), c(1,1,1))
    colnames(votes_matrix) <- c("A", "B", "C")
    expect_error(district_winner_matrix(votes_matrix, ties = "error"), "Tied majority in 'B'")
    expect_error(district_winner_matrix(votes_matrix, ties = "x"), "ties param must be")

})

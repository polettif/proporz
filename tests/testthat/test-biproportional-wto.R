test_that("col/row max", {
    m = matrix(c(8,9,2,1), 2)
    expect_equal(row_maxs(m), c(8,9))
    expect_equal(col_maxs(m), c(9,2))
})

test_that("most_votes_in_district_matrix", {
    votes_matrix = matrix(c(90, 50, 60, 50, 10, 50), ncol = 3)
    colnames(votes_matrix) <- c("A", "B", "C")
    dw = most_votes_in_district_matrix(votes_matrix)
    dw <- unname(dw)

    expect_equal(dw, matrix(c(T,F,T,F,F,T), ncol = 3))
    expect_true(is.logical(dw))
    expect_equal(colSums(dw), c(1,1,1))
    expect_equal(sum(colSums(dw)), 3)
})

test_that("winner take one", {
    expect_error(
        biproporz(matrix(1:9, ncol = 3), 1:3, method = "wto"),
        "votes_matrix must have column and row names to handle district winners")

    vm = matrix(c(60,10,10,11), 2, dimnames = list(as.character(1:2), c("A", "B")))
    expect_error(biproporz(vm, setNames(c(1,1), colnames(vm)), method = "wto"),
                 "Not enough upper apportionment seats to give district winner seats to party/list: '2'")

    vm2 = matrix(c(200,100,10,11), 2, dimnames = list(as.character(1:2), c("A", "B")))
    seats2 = setNames(c(2,1), colnames(vm))
    bp1 = biproporz(vm2, seats2, method = "round")
    bp2 = biproporz(vm2, seats2, method = "wto")

    expect_equal(c(bp1), c(1,1,1,0))
    expect_equal(c(bp2), c(2,0,0,1))

    # pukelsheim
    df = pivot_to_df(vm2)
    seatsdf = data.frame(district = names(seats2), seats = seats2)
    pk1 = pukelsheim(df, seatsdf, winner_take_one = FALSE)
    expect_equal(matrix(pk1[["seats"]], 2, 2, byrow = T), as.matrix(unname(bp1)))
    pk2 = pukelsheim(df, seatsdf, winner_take_one = TRUE)
    expect_equal(matrix(pk2[["seats"]], 2, 2, byrow = T), as.matrix(unname(bp2)))
})

test_that("two with ties and enough seats", {
    vm12 = matrix(c(60,10,20,10,11,11), 3, dimnames = list(as.character(1:3), c("A", "B")))
    seats1 = setNames(c(2,1), colnames(vm12))
    seats2 = setNames(c(2,2), colnames(vm12))

    expect_warning(biproporz(vm12, seats1, method = "wto"),
                   "Not enough seats for tied parties with the most votes in: 'B'")
    expect_s3_class(biproporz(vm12, seats2, method = "wto"), "proporz_matrix")

    vm3 = vm12
    vm3[3,1] <- 0
    expect_warning(biproporz(vm3, seats1, method = "wto"),
                   "Not enough seats for tied parties with the most votes in: 'B'")
    expect_error(biproporz(vm3, seats2, method = "wto"),
                 "Not enough upper apportionment seats to give district winner seats to party/list: '3'")
})

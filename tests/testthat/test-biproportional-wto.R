test_that("col/row max", {
    m = matrix(c(8,9,2,1), 2)
    expect_equal(row_maxs(m), c(8,9))
    expect_equal(col_maxs(m), c(9,2))
})

test_that("district_winner_matrix", {
    votes_matrix = matrix(c(90, 50, 60, 50, 10, 50), ncol = 3)
    expect_error(district_winner_matrix(votes_matrix), "votes matrix must have district column names")
    colnames(votes_matrix) <- c("A", "B", "C")
    dw = district_winner_matrix(votes_matrix)
    dw <- unname(dw)

    expect_equal(dw, matrix(c(T,F,T,F,F,T), ncol = 3))
    expect_true(is.logical(dw))
    expect_equal(colSums(dw), c(1,1,1))
    expect_equal(sum(colSums(dw)), 3)

    # ties
    votes_matrix[1,2] <- 50
    expect_error(district_winner_matrix(votes_matrix), "Tied majority in 'B'")
    votes_matrix[2,3] <- 10
    expect_error(district_winner_matrix(votes_matrix), "Tied majority in 'B', 'C'")
})

test_that("winner take one", {
    expect_error(
        biproporz(matrix(1:9, ncol = 3), 1:3, method = "wto"),
        "votes_matrix must have column and row names to handle district winners")

    vm = matrix(c(60,10,10,11), 2, dimnames = list(as.character(1:2), c("A", "B")))
    expect_error(biproporz(vm, setNames(c(1,1), colnames(vm)), method = "wto"),
                 "Not enough upper apportionment seats to give district winner seats to party/list 2")

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

test_that("wto with grisons2022 dataset", {
    # Grisons 2022
    seats_expected = structure(
        c(0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 2, 1, 1, 3, 1, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 2, 1, 1,
          0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 4, 0, 1, 0, 1, 1, 1, 1, 1,
          0, 0, 1, 1, 0, 2, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0,
          1, 0, 3, 0, 1, 0, 1, 4, 1, 0, 1, 1, 1, 0, 1, 0, 2, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1,
          1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 0, 0, 1, 0, 1, 3, 1, 1, 3,
          0, 2, 3, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 3, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1),
        dim = c(39L, 5L),
        dimnames = list(
            c("Alvaschein", "Avers", "Belfort", "Bergün", "Bregaglia", "Breil/Brigels",
            "Brusio", "Calanca", "Chur", "Churwalden", "Davos", "Disentis", "Domleschg",
            "Fünf Dörfer", "Ilanz", "Jenaz", "Klosters", "Küblis", "Lumnezia/Lugnez",
            "Luzein", "Maienfeld", "Mesocco", "Oberengadin", "Poschiavo", "Ramosch",
            "Rhäzüns", "Rheinwald", "Roveredo", "Safien", "Schams", "Schanfigg", "Schiers",
            "Seewis", "Suot Tasna", "Sur Tasna", "Surses", "Thusis", "Trins", "Val Müstair"),
            c("SP&Grüne", "FDP", "SVP", "GLP", "Mitte")))

    expect_true(has_tied_district_winners(t(grisons2022$votes_matrix)))
    expect_error(biproporz(t(grisons2022$votes_matrix), grisons2022$district_seats_df,
                           method = "wto"), "Tied majority in 'Rheinwald'")

    gr2022 = grisons2022$votes_matrix
    # fix tie, the winner was actually chosen by lot
    gr2022["Rheinwald", "SP&Grüne"] <- gr2022["Rheinwald","SP&Grüne"]+1
    # one vote more doesnt' influence the result (0.0003% of all list votes)
    gr2022 <- t(gr2022)

    seats_wto = biproporz(gr2022, grisons2022$district_seats_df,
                          quorum = quorum_all(total = 0.03),
                          method = "wto")
    seats_actual = as.matrix(t(seats_wto))
    expect_equal(seats_actual, seats_expected)
})

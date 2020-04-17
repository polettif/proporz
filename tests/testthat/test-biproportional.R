context("biproportional")

# https://en.wikipedia.org/wiki/Biproportional_apportionment
M1 = matrix(c(123,912,312,45,714,255,815,414,215), nrow = 3)
d1 = c(7,5,8)
p1 = c(5,11,4)

# https://de.wikipedia.org/wiki/Doppeltproportionales_Zuteilungsverfahren
M2 = matrix(c(51,60,63,98,100,102,45,120,144), nrow = 3)
d2 = 4:6
p2 = 4:6

# https://de.wikipedia.org/wiki/Sitzzuteilungsverfahren#Biproportionales_Verfahren
test_that("upper apportionment", {
    expect_equal(upper_apportionment(M1, d1), p1)
    expect_equal(upper_apportionment(M2, d2), p2)
})

test_that("lower apportionment", {
    x1 = lower_apportionment(M1, d1, p1)
    expect_equal(c(x1), c(1,4,2,0,4,1,4,3,1))

    x2 = lower_apportionment(M2, d2, p2)
    expect_equal(c(x2), c(1,1,2,2,2,1,1,2,3))
})

test_that("biproportional", {
    act = biproportional(M2, d2)
    act <- as.matrix(act)
    exp = matrix(c(1,1,2,2,2,1,1,2,3), nrow=3)
    expect_equal(act, exp)
    expect_is(act, "matrix")
})

test_that("pukelsheim wrapper", {
    pklshm = data.frame(
        Liste = rep(1:3, each = 3),
        Wahlkreis = rep(c("A", "B", "C"), 3),
        Stimmen = c(51,98,45,60,100,120,63,102,144),
        stringsAsFactors = FALSE)
    pklshm_seats = data.frame(
        Wahlkreis = c("A", "B", "C"),
        Sitze = 4:6,
        stringsAsFactors = FALSE
    )
    expect_error(pukelsheim(pklshm[,2:3], pklshm_seats))
    expect_error(pukelsheim(pklshm[,c(2,1,3)], pklshm_seats))
    result = pukelsheim(pklshm, pklshm_seats, "Sitze")
    expect_equal(result[,1:3], pklshm)
    expect_equal(result$Sitze, c(1,2,1,1,2,2,2,1,3))
    expect_true(!is.null(divisors(result)))
})

test_that("biproportional with names", {
    uri20_districts = c("Altdorf" = 15, "Bürglen" = 7,
                        "Erstfeld" = 6, "Schattdorf" = 9)
    uri20_input = structure(c(11471L, 11908L, 9213L, 7756L, 2822L,
                              1606L, 1567L, 2945L, 2309L, 1705L,
                              946L, 1573L, 4794L, 2600L, 2961L, 3498L),
                            .Dim = c(4L, 4L), .Dimnames = list(
                                c("CVP", "SPGB", "FDP", "SVP"),
                                names(uri20_districts)))
    uri20_result = structure(c(5L, 4L, 3L, 3L, 2L, 1L, 1L,3L,
                               2L, 2L, 1L, 1L, 3L, 2L, 2L, 2L),
                             .Dim = c(4L, 4L), .Dimnames = list(
                                 c("CVP", "SPGB", "FDP", "SVP"),
                                 names(uri20_districts)))

    uri20_output = biproportional(uri20_input, uri20_districts)
    expect_equal(unname(as.matrix(uri20_output)), unname(uri20_result))
    expect_equivalent(rownames(uri20_output), rownames(uri20_result))
    expect_equivalent(colnames(uri20_output), colnames(uri20_result))
    expect_true(!is.null(divisors(uri20_output)))
})

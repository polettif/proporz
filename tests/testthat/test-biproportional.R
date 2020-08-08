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
test_that("upper apportionment (party seats)", {
    expect_equal(upper_apportionment(M1, d1)$party, p1)
    expect_equal(upper_apportionment(M2, d2)$party, p2)
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

test_that("quorum with vote counts", {
    vm0 = matrix(c(30, 10, 60, 50, 20, 180), nrow = 3)
    for(i in 1:3) {
        vm = vm0
        vm[c(1,3),] <- round(runif(2, 1, 10)*vm[c(1,3),])
        expect_equal(biprop_quorum(vm), vm)
        q1 = biprop_quorum(vm, quorum_districts = 35)
        check1 = all(q1[2,] == c(0,0))
        q2 = biprop_quorum(vm, quorum_total = 45)
        check2 = all(q2[2,] == c(0,0))
        q3 = biprop_quorum(vm, quorum_districts = 35, quorum_total = 45)
        check3 = identical(q2, q3)
        expect_true(all(check1, check2, check3))
    }
})

test_that("quorum with percentages counts", {
    vm = matrix(c(30, 10, 60, 50, 20, 180), nrow = 3)
    p1 = biprop_quorum(vm, quorum_districts = 0.15)
    expect_equal(p1[2,], c(0,0))
    p2 = biprop_quorum(vm, quorum_districts = 0.09)
    expect_true(all(p2[2,] != c(0,0)))
    p3 = biprop_quorum(vm, quorum_total = 0.085)
    expect_equal(p2,p3)
    p4 = biprop_quorum(vm, quorum_total = 0.09)
    expect_equal(p1, p4)
})

test_that("free apportionment for districts", {
    # https://en.wikipedia.org/wiki/Biproportional_apportionment#Specific_Example
    input_matrix = matrix(c(123,912,312,45,714,255,815,414,215), nrow = 3)
    mtrx_exp = matrix(c(1,4,2,0,4,1,4,3,1), nrow = 3)
    mtrx_act = biproportional(input_matrix, 20)
    mtrx_act <- as.matrix(mtrx_act)
    expect_equal(mtrx_act, mtrx_exp)
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

suomi19_votes = structure(
    list(list_id = c("SDP", "PS", "KOK", "PS", "KOK", "SDP", "KOK", "SDP", "PS"),
         entity_id = c("HEL", "UUS", "HAEM", "HEL", "UUS", "HAEM", "HEL", "UUS", "HAEM"),
         list_votes = c(4000, 9000, 17000, 23000, 29500, 36000, 42500, 49000, 17999)),
    row.names = c(NA, -9L), class = "data.frame")
suomi19_distr_seats = structure(
    list(entity_id = c("HEL", "UUS", "HAEM"),
         election_mandates = c(5, 10, 15)),
    row.names = c(NA, -3L), class = "data.frame")

test_that("expand divisor range", {
    suomi19_listvotes = pukelsheim(suomi19_votes, suomi19_distr_seats)
    expect_equal(sum(suomi19_listvotes$seats), sum(suomi19_distr_seats$election_mandates))
})

test_that("use_list_votes=FALSE", {
    # divisor round with sainte-lague
    vm_19 = pivot_to_matrix(suomi19_votes)
    votes_vec = rowSums(vm_19)
    seats_vec = divisor_round(votes_vec, 30)

    # compare with pukelsheim using raw voter data
    seats_df = pukelsheim(suomi19_votes, suomi19_distr_seats, use_list_votes = FALSE)
    seats_mtrx = pivot_to_matrix(seats_df[c(1,2,4)])
    expect_equal(seats_vec, rowSums(seats_mtrx))
})

test_that("biproportional methods", {
    vm_19 = pivot_to_matrix(suomi19_votes)
    bip19 = biproportional(vm_19, suomi19_distr_seats,
                   use_list_votes = FALSE,
                   method = c("floor", "round"))
    dhondt19 = proporz(rowSums(vm_19), 30, "d'hondt")
    expect_equal(rowSums(bip19), dhondt19)
})

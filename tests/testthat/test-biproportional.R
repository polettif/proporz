expect_error_fixed = function(...) {
    testthat::expect_error(..., fixed = TRUE)
}

# basic apportionment steps ####

# https://en.wikipedia.org/wiki/biproporz_apportionment
M1 = matrix(c(123,912,312,45,714,255,815,414,215), nrow = 3)
d1 = c(7,5,8)
p1 = c(5L,11L,4L)

# https://de.wikipedia.org/wiki/Doppeltproportionales_Zuteilungsverfahren
M2 = matrix(c(51,60,63,98,100,102,45,120,144), nrow = 3)
d2 = 4:6
p2 = 4:6

# https://de.wikipedia.org/wiki/Sitzzuteilungsverfahren#Biproportionales_Verfahren
test_that("upper apportionment (party seats)", {
    expect_identical(upper_apportionment(M1, d1)$party, p1)
    expect_identical(upper_apportionment(M2, d2)$party, p2)
})

test_that("lower apportionment", {
    x1 = lower_apportionment(M1, d1, p1)
    expect_identical(c(x1), as.integer(c(1,4,2,0,4,1,4,3,1)))

    x2 = lower_apportionment(M2, d2, p2)
    expect_identical(c(x2), as.integer(c(1,1,2,2,2,1,1,2,3)))

    x3 = lower_apportionment(M2, d2, p2, method = function(x) ceil_at(x, 0.5))
    expect_identical(x3, x2)

    expect_identical(sum(
        lower_apportionment(matrix(c(1,0,1,0), 2), c(1,1), c(2,0), method = "harmonic")),
        2L)

    # exact 0.5 seats edge case
    vm0.5 = matrix(c(10, 10, 20, 10), 2, 2)
    sm0.5 = lower_apportionment(vm0.5, c(1, 1), c(1,1))
    expect_identical(sum(sm0.5), 2L)
})

test_that("biproporz", {
    act = biproporz(M2, d2)
    act <- as.matrix(act)
    exp = matrix(c(1L,1L,2L,2L,2L,1L,1L,2L,3L), nrow=3)
    expect_identical(act, exp)
    expect_is(act, "matrix")
})

test_that("weight_list_votes", {
    vm = matrix(c(110,50,20,10), 2)
    vmw = weight_list_votes(vm, c(10, 2))
    expect_equal(vmw, matrix(c(110/10,50/10,20/2,10/2), 2), tolerance = 1e-14)
})

# expanded usage ####
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

    x = pklshm[,2:3]
    expect_error_fixed(
        pukelsheim(x, pklshm_seats),
        "`x` must be a data frame with 3 columns in the following order:\nparty, district and votes (names can differ)")

    x = pklshm[,c(2,1,3)]
    expect_error_fixed(pukelsheim(x, pklshm_seats),
                       "District ids not found in second column of `x`. Are columns in the correct order (party, district, votes)?")

    result = pukelsheim(pklshm, pklshm_seats, new_seats_col = "Sitze")
    expect_identical(result[,1:3], pklshm)
    expect_identical(result$Sitze, as.integer(c(1,2,1,1,2,2,2,1,3)))
    expect_true(!is.null(get_divisors(result)))
})

test_that("free apportionment for districts", {
    # https://en.wikipedia.org/wiki/biproporz_apportionment#Specific_example
    input_matrix = matrix(c(123,912,312,45,714,255,815,414,215), nrow = 3)
    mtrx_exp = matrix(as.integer(c(1,4,2,0,4,1,4,3,1)), nrow = 3)
    mtrx_act = biproporz(input_matrix, 20)
    mtrx_act <- as.matrix(mtrx_act)
    expect_identical(mtrx_act, mtrx_exp)

    # pukelsheim
    votes_df = unique(zug2018[c("list_id", "entity_name", "list_votes")])
    votes_matrix = pivot_to_matrix(votes_df)
    x = biproporz(votes_matrix, 80)
    expect_identical(sum(x), 80L)
})

test_that("named votes_matrix", {
    votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)
    dimnames(votes_matrix) <- list(c("A", "B", "C", "D"), c("Z1", "Z2"))

    expect_error_fixed(biproporz(votes_matrix, c(50, 20)),
                       "needs to have the same names as the columns in `votes_matrix`")
    expect_error_fixed(biproporz(unname(votes_matrix), c(Z1 = 50, Z2 = 20)),
                       "needs to have the same names as the columns in `unname(votes_matrix)`.")
    expect_error_fixed(biproporz(votes_matrix, c(Z0 = 50, Z2 = 20)),
                       "needs to have the same names as the columns in `votes_matrix`.")

    seats = c("Z2" = 20, "Z1" = 50)
    expect_identical(biproporz(votes_matrix, seats), biproporz(votes_matrix, seats[2:1]))
    expect_identical(biproporz(votes_matrix, seats)[,c("Z1", "Z2")],
                     biproporz(votes_matrix[,2:1], seats[2:1])[,c("Z1", "Z2")])
})

test_that("flow criterion check for almost empty matrix", {
    vm1 = matrix(c(10,0,0,0, 0,0,10,0, 0,0,0,20, 0,0,0,20), nrow = 4)
    expect_identical(biproporz(vm1, 4), biproporz(vm1, rep(1,4)))

    vm2 = matrix(0, nrow = 4, ncol = 4)
    vm2[1,2] <- 10
    expect_identical(sum(biproporz(vm2, 2)), 2L)
    expect_error_fixed(biproporz(vm2, c(1,1,0,0)),
                       "No votes in a district with at least one seat")

    expect_error_fixed(
        biproporz(matrix(c(0, 1, 0, 0, 4, 3, 0, 0, 20), 3), c(4,1,3)),
        "Not enough seats for party 3 in districts 2, 3\n(6 seats necessary, 4 available)")

    expect_error_fixed(
        lower_apportionment(matrix(c(5,0,0,0,15,16), nrow = 3), c(3,1), c(2,1,1)), # almost impossible to trigger with biproporz
        "Not enough seats for parties 2, 3 in district 2\n(2 seats necessary, 1 available)")

    expect_error_fixed(
        lower_apportionment(matrix(c(0, 10, 15, 0, 0, 20, 10, 0, 10, 0, 0, 20), 4),
                            c(3,1,1), c(2,1,1,1)),
        "Not enough seats for parties 1, 4 in district 3\n(3 seats necessary, 1 available)")

    expect_error_fixed(
        biproporz(matrix(c(1000,10,0,1), 2), c(1,1)),
        "Not enough seats for party 1 in district 1\n(2 seats necessary, 1 available)")

    vm3a = matrix(c(4,3,0,20,1,0), nrow = 2)
    expect_error_fixed(
        biproporz(vm3a, c(1,3,4)),
        "Not enough seats for party 2 in districts 1, 2\n(6 seats necessary, 4 available)")

    vm3b = vm3a[,c(1,3,2)]
    rownames(vm3b) <- c("ONE", "TWO")
    expect_error_fixed(
        biproporz(vm3b, c(1,4,3)),
        "Not enough seats for party 'TWO' in districts 1, 3\n(6 seats necessary, 4 available)")
    colnames(vm3b) <- c("A", "B", "C")
    expect_error_fixed(
        biproporz(vm3b, c(A=1,B=4,C=3)),
        "Not enough seats for party 'TWO' in districts 'A', 'C'\n(6 seats necessary, 4 available)")

    # check in multiple districts
    expect_error_fixed(
        check_flow_criterion(matrix(c(1,1,1,0,0,0, 1,0,0,1,1,0, 0,0,0,0,0,1), nrow = 6),
                             c(2,2,2), c(1,1,1,1,1,1)),
        "Not enough seats for parties 1, 2, 3, 4, 5 in districts 1, 2\n(5 seats necessary, 4 available)")

    vm_blocks1 = matrix(c(12L, 10L, 0L, 0L, 5L, 5L, 0L, 0L, 0L, 0L, 6L, 10L, 0L, 0L, 10L, 5L),
                        nrow = 4L, ncol = 4L,
                        dimnames = list(
                            party = c("A", "B", "C", "D"),
                            district = c("District 1", "District 2", "District 3", "District 4")))
    seats_blocks1 = c(`District 1` = 5L, `District 2` = 5L, `District 3` = 5L, `District 4` = 6L)
    expect_error_fixed(biproporz(vm_blocks1, seats_blocks1),
                       "Not enough seats for parties 'A', 'B' in districts 'District 1', 'District 2'\n(11 seats necessary, 10 available")

    vm_blocks2 = vm_blocks1[c(4,3,1,2),c(1,3,2,4)]
    dimnames(vm_blocks2) <- list(LETTERS[1:4], as.character(1:4))
    seats_blocks2 = setNames(seats_blocks1[c(1,3,2,4)], colnames(vm_blocks2))
    expect_error_fixed(biproporz(vm_blocks2, seats_blocks2),
                       "Not enough seats for parties 'C', 'D' in districts '1', '3'\n(11 seats necessary, 10 available)")

    vm_blocks3 = uri2020$votes_matrix
    vm_blocks3[1,c(1,2,4)] <- vm_blocks3[4,c(1,2,4)] <- 0
    expect_error(biproporz(vm_blocks3, uri2020$seats_vector),
                 "Not enough seats for parties 'CVP', 'SVP' in district 'Erstfeld'")

     # no submatrix error for matrix with diag = 0
    vm_diag = matrix(c(0, 20, 100, 20, 0, 20, 100, 100, 0), nrow = 3)
    expect_is(biproporz(vm_diag, c(50, 30, 90)), "proporz_matrix")
})

test_that("flow criterion helper", {
    M = matrix(c(T,T,F,T,F,F,T,F,F,F,T,F,F,F,T), byrow = TRUE, ncol = 3)
    expect_false(is_flow_criterion_pair(c(FALSE,FALSE),c(TRUE,FALSE)))
    expect_false(is_flow_criterion_pair(c(TRUE,FALSE),c(FALSE,FALSE)))
    expect_equal(apply(M, 1, is_flow_criterion_pair, M[1,]), c(TRUE, TRUE, TRUE, TRUE, FALSE))
    expect_equal(apply(M, 1, is_flow_criterion_pair, M[2,]), c(FALSE, TRUE, TRUE, FALSE, FALSE))
    expect_equal(apply(M, 1, is_flow_criterion_pair, M[5,]), c(FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("undefined result biproportional", {
    seats = c(10, 20, 1, 1)
    set.seed(1284)
    vm = matrix(runif(4*10), ncol = 4) * matrix(rep(seats, 10), byrow = TRUE, ncol = 4) * 1000
    vm <- round(vm)
    vm[vm < 200] <- 0

    expect_identical(upper_apportionment(vm, seats, use_list_votes = FALSE)$party,
                     proporz(rowSums(vm), sum(seats), "round"))

    expect_error_fixed(upper_apportionment(vm, seats),
                       "Result is undefined, equal quotient for parties: 4, 6")

    expect_error_fixed(biproporz(uri2020$votes_matrix, uri2020$seats_vector, quorum_any(any_district = 0.7)),
                       "Result is undefined, equal quotient for parties: 'CVP', 'SPGB', 'FDP', 'SVP'")

    vm5 = matrix(c(10, 10, 10, 10), 2, 2)
    expect_error_fixed(biproporz(vm5, c(3,1)),
                       "Result is undefined, cannot assign all seats in lower apportionment")

    # manual fix (actual implementation depends on rules)
    vm4 <- vm6 <- vm
    vm4[4,1] <- vm4[4,1]+1
    vm6[6,4] <- vm6[6,4]+1
    ua4 = upper_apportionment(vm4, seats)
    ua6 = upper_apportionment(vm6, seats)
    expect_identical(ua4$party[4], ua6$party[6])

    # fully tied
    vdf = data.frame(
        party = rep(c("A", "B", "C", "D", "E"), 5),
        district = rep(c("d1", "d2", "d3", "d4", "d5"), each = 5L),
        votes = rep(c(0.2, 0.5, 0.2, 0.5, 0.2, 0.5, 0.2, 0.5, 0.2),
                    c(2L, 3L, 2L, 5L, 3L, 2L, 3L, 2L, 3L)))
    vdf_seats = data.frame(
        district = c("d1", "d2", "d3", "d4", "d5"),
        seats = rep(1L, 5L))
    expect_error(pukelsheim(vdf, vdf_seats), "Result is undefined, tied votes and multiple possible seat assignments")
})

test_that("find_divisor", {
    v = matrix(c(80,10,10))
    .check = function(div) round(v/div)

    d0 = find_divisor(v, 0, 100, 10, function(x) ceil_at(x, 0.5))
    expect_identical(.check(d0), .check(10))
    # expand lower limit
    d1 = find_divisor(v, 20, 100, 10, function(x) ceil_at(x, 0.5))
    expect_identical(.check(d1), .check(10))
    # expand upper limit
    d2 = find_divisor(v, 1, 5, 10, function(x) ceil_at(x, 0.5))
    expect_identical(.check(d2), .check(10))
})

test_that("districts with one seat", {
    seats = c(10, 20, 1, 1)
    set.seed(80)

    votes_matrix = matrix(runif(4*10), ncol = 4) * matrix(rep(seats, 10), byrow = TRUE, ncol = 4) * 100
    votes_matrix <- round(votes_matrix)
    votes_matrix[votes_matrix < 30] <- 0

    expect_identical(colSums(biproporz(votes_matrix, seats)), seats)
})

# quorum ####
test_that("quorum with vote counts", {
    vm0 = matrix(c(30, 10, 60, 50, 20, 180), nrow = 3)
    vm = vm0
    vm[c(1,3),] <- round(c(7.97, 5.14)*vm[c(1,3),])

    q1 = apply_quorum_matrix(vm, quorum_any(any_district = 35))
    check1 = all(q1[2,] == c(0,0))
    q2 = apply_quorum_matrix(vm, quorum_any(total = 45))
    check2 = all(q2[2,] == c(0,0))
    q3 = apply_quorum_matrix(vm, quorum_all(any_district = 35, total = 45))
    check3 = identical(q2, q3)
    expect_true(all(check1, check2, check3))

    expect_identical(reached_quorum_any_district(vm, 35), rowSums(q1) > 0)
    expect_identical(reached_quorum_total(vm, 45), rowSums(q2) > 0)

    expect_identical(sum(apply_quorum_matrix(vm, c(FALSE,FALSE,FALSE))), 0)
    expect_identical(sum(apply_quorum_matrix(vm, c(FALSE,TRUE,FALSE))), 30)
    expect_error_fixed(apply_quorum_matrix(vm, "x"), "Cannot parse quorum function or vector.")
})

test_that("quorum with percentages counts", {
    vm = matrix(c(30, 10, 60, 50, 20, 180), nrow = 3)
    p1 = apply_quorum_matrix(vm, quorum_any(any_district = 0.15))
    expect_identical(p1[2,], c(0,0))
    p2 = apply_quorum_matrix(vm, quorum_any(any_district = 0.09))
    expect_true(all(p2[2,] != c(0,0)))
    p3 = apply_quorum_matrix(vm, quorum_any(total = 0.085))
    expect_identical(p2,p3)
    p4 = apply_quorum_matrix(vm, quorum_any(total = 0.09))
    expect_identical(p1, p4)
})

test_that("quorum param", {
    vm = matrix(c(90, 4, 5, 1, 104, 4, 1, 1), ncol = 2)*10

    q_district = c(TRUE,FALSE,TRUE,FALSE)
    q_total = c(TRUE,TRUE,FALSE,FALSE)
    q_district_and_total = c(TRUE,FALSE,FALSE,FALSE)
    q_district_or_total = c(TRUE,TRUE,TRUE,FALSE)

    # reached_quorums
    expect_identical(reached_quorums(vm, quorum_any(any_district = 0.05)),
                     q_district)
    expect_identical(reached_quorums(vm, quorum_all(any_district = 0.05)),
                     q_district)
    expect_identical(reached_quorums(vm, quorum_any(total = 0.03)),
                     q_total)
    expect_identical(reached_quorums(vm, quorum_all(total = 0.03)),
                     q_total)
    expect_identical(reached_quorums(vm, quorum_any(any_district = 0.05, total = 0.03)),
                     q_district_or_total)
    expect_identical(reached_quorums(vm, quorum_all(any_district = 0.05, total = 0.03)),
                     q_district_and_total)
    expect_identical(reached_quorums(vm, quorum_all(any_district = 0.05, total = 63)),
                     q_district_and_total)
    expect_error_fixed(reached_quorums(vm, reached_quorum_total), "`reached_quorum_total` is not a list of functions.")
    expect_error_fixed(reached_quorums(vm, list(1, 2)), "`list(1, 2)` is not a list of functions.")

    dummy_list = list(function(x) x > 0, function(x) x < 0)
    expect_error_fixed(reached_quorums(vm, dummy_list), "type must be set as list attribute.")
    attributes(dummy_list)$type <- "either_or"
    expect_error_fixed(reached_quorums(vm, dummy_list), "Unknown type `either_or`.")

    # vote_matrix
    soll_district = vm * matrix(rep(q_district, 2), ncol = 2)
    soll_total = vm * matrix(rep(q_total, 2), ncol = 2)
    soll_district_and_total = vm * matrix(rep(q_district_and_total, 2), ncol = 2)
    soll_district_or_total = vm * matrix(rep(q_district_or_total, 2), ncol = 2)

    # biproporz()
    expect_identical(
        biproporz(vm, c(100, 100), quorum_any(any_district = 0.05)) > 0,
        soll_district > 0)
    expect_identical(
        biproporz(vm, c(100, 100), quorum_any(any_district = 0.05)) > 0,
        soll_district > 0)

    expect_identical(
        biproporz(vm, c(100, 100), quorum_any(total = 0.03)) > 0,
        soll_total > 0)
    expect_identical(
        biproporz(vm, c(100, 100), quorum_all(any_district = 0.05, total = 0.03)) > 0,
        soll_district_and_total > 0)
    expect_identical(
        biproporz(vm, c(100, 100), quorum_any(any_district = 0.05, total = 0.03)) > 0,
        soll_district_or_total > 0)

    # with vector
    expect_identical(
        biproporz(vm, c(100, 100), q_district_or_total) > 0,
        soll_district_or_total > 0)

    # pukelsheim()
    vm_df = pivot_to_df(vm)
    wrap_pukelsheim = function(...) {
        seats_df = data.frame(col = c(1,2), seats = c(100, 100))
        unname(pivot_to_matrix(pukelsheim(vm_df, seats_df, ...)[c(1,2,4)]))
    }

    expect_identical(
        wrap_pukelsheim(quorum = quorum_any(any_district = 0.05)) > 0,
        soll_district > 0)
    expect_identical(
        wrap_pukelsheim(quorum = quorum_any(total = 0.03)) > 0,
        soll_total > 0)
    expect_identical(
        wrap_pukelsheim(quorum = quorum_all(any_district = 0.05, total = 0.03)) > 0,
        soll_district_and_total > 0)
    expect_identical(
        wrap_pukelsheim(quorum = quorum_any(any_district = 0.05, total = 0.03)) > 0,
        soll_district_or_total > 0)

    # biproportional quorum edge case
    ec_vm = matrix(c(89,5,2,4, 96,1,1,4), ncol = 2)
    ec_bp = biproporz(vm, c(93, 100), quorum_any(any_district = 0.05))
    expect_is(ec_bp, "proporz_matrix")
})

# catch edge with more complicated data set ####
suomi19_votes = structure(
    list(list_id = c("SDP", "PS", "KOK", "PS", "KOK", "SDP", "KOK", "SDP", "PS"),
         entity_id = c("HEL", "UUS", "HAEM", "HEL", "UUS", "HAEM", "HEL", "UUS", "HAEM"),
         list_votes = c(4000, 9000, 17000, 23000, 29500, 36000, 42500, 49000, 17999)),
    row.names = c(NA, -9L), class = "data.frame")
suomi19_distr_seats = structure(
    list(entity_id = c("HEL", "UUS", "HAEM"),
         election_mandates = c(5L, 10L, 15L)),
    row.names = c(NA, -3L), class = "data.frame")

test_that("expand divisor range", {
    suomi19_listvotes = pukelsheim(suomi19_votes, suomi19_distr_seats)
    expect_identical(sum(suomi19_listvotes$seats), sum(suomi19_distr_seats$election_mandates))
})

test_that("use_list_votes=FALSE", {
    # divisor round with sainte-lague
    vm_19 = pivot_to_matrix(suomi19_votes)
    votes_vec = rowSums(vm_19)
    seats_vec = divisor_round(votes_vec, 30)

    # compare with pukelsheim using raw voter data
    seats_df = pukelsheim(suomi19_votes, suomi19_distr_seats, use_list_votes = FALSE)
    seats_mtrx = pivot_to_matrix(seats_df[c(1,2,4)])
    expect_equal(seats_vec, rowSums(seats_mtrx), tolerance = 1e-14)
})

test_that("different method for upper and lower app", {
    vm_19 = pivot_to_matrix(suomi19_votes)
    bip19 = biproporz(vm_19, suomi19_distr_seats,
                      use_list_votes = FALSE,
                      method = c("floor", "round"))
    dhondt19 = proporz(rowSums(vm_19), 30, "d'hondt")
    expect_equal(rowSums(bip19), dhondt19, tolerance = 1e-14)
    bip19_list = biproporz(vm_19, suomi19_distr_seats,
                           use_list_votes = FALSE,
                           method = list("floor", "round"))
    expect_identical(bip19_list, bip19)
})

# Error messages ####
test_that("error messages", {
    set.seed(2)
    vm = matrix(round(runif(12, 50, 1500)), nrow = 3)
    seats = c(10,20,10,9)

    # pukelsheim
    vdf = pivot_to_df(vm)
    colnames(vdf) <- c("party", "district", "votes")
    seats_df = data.frame(col = 1:4, seats = seats)
    seats_df124 = seats_df[-3,]
    vdf134 = vdf[vdf[["district"]] != 2,]
    expect_true(nrow(vdf134) > 0)

    # unique party ids
    vdf_dupl = rbind(vdf, vdf[9:12,])
    expect_error_fixed(pukelsheim(vdf_dupl, seats_df),
                       "There are duplicate party-district pairs in `vdf_dupl`.")

    # unique district ids
    expect_error_fixed(pukelsheim(vdf_dupl, rbind(seats_df, seats_df)),
                       "District ids in `rbind(seats_df, seats_df)` are not unique.")

    # more helpful message if columns are switched
    expect_error_fixed(pukelsheim(vdf[,c(2,1,3)], seats_df),
                       paste0("District ids not found in second column of `vdf[, c(2, 1, 3)]`. ",
                              "Are columns in the correct order (party, district, votes)?"))

    # districts not in vote_districts
    expect_error_fixed(pukelsheim(vdf134, seats_df),
                       paste0("Not all district ids in `seats_df`s first column exist ",
                              "in `vdf134`s second column."))

    # vote_districts not in districts
    expect_error_fixed(pukelsheim(vdf, seats_df124),
                       "Not all district ids in `vdf`s second column exist in `seats_df124`s first column")

    # mismatch on both ends
    expect_error_fixed(pukelsheim(vdf134, seats_df124),
                       "Not all district ids in `seats_df124`s first column exist in `vdf134`s second column")

    # seats_df is not a data.frame
    seats_vec124 = setNames(seats_df124$seats, seats_df124$col)
    expect_error_fixed(pukelsheim(vdf134, seats_vec124), "`seats_vec124` must be a data.frame.")

    # non-numeric
    vdf_non_num = vdf
    vdf_non_num$votes <- as.character(vdf_non_num$votes)
    expect_error_fixed(pukelsheim(vdf_non_num, seats_df),
                       "Vote values in `vdf_non_num`s third column must be numbers >= 0")

    # negative votes
    vdf_neg = vdf
    vdf_neg$votes <- vdf_neg$votes-500
    expect_error_fixed(pukelsheim(vdf_neg, seats_df),
                       "Vote values in `vdf_neg`s third column must be numbers >= 0")
    vm_neg = vm
    vm_neg[2:3,2] <- -vm_neg[2:3,2]
    expect_error_fixed(biproporz(vm_neg, c(2,3,2,1)), "Votes in `vm_neg` must be numbers >= 0")
    vm_char = matrix(as.character(vm), nrow = nrow(vm))
    expect_error_fixed(biproporz(vm_char, c(2,3,2,1)), "Votes in `vm_char` must be numbers >= 0")

    # biproportional
    expect_error_fixed(biproporz(vm, NA), "`NA` must be a numeric vector, data.frame or a single number.")
    expect_error_fixed(biproporz(vdf, c(1,2,3)), "`vdf` must be a matrix.")
    expect_error_fixed(biproporz(vm, c(1,2,3)), "`vm` needs to have districts as columns and parties as rows.")
    expect_error_fixed(biproporz(vm, seats, method = "largest_remainder_method"),
                       'Cannot use "largest_remainder_method", only divisor methods are possible in biproportional apportionment.')
    expect_s3_class(biproporz(vm+0.1, seats), "proporz_matrix")
    expect_s3_class(biproporz(vm*0.1, seats), "proporz_matrix")
    expect_identical(as.matrix(biproporz(vm*0.01, seats)), as.matrix(biproporz(vm*20, seats)))
    expect_error_fixed(biproporz(vm, seats+0.1), "`seats + 0.1` must be integers.")
    expect_error_fixed(biproporz(vm, seats, method = c("round", "floor", "ceiling")),
                       "Only one or two methods allowed.")
    expect_error_fixed(biproporz(vm, vm), "`vm` must be a numeric vector, data.frame or a single number.")

    # upper/lower_apportionment
    ua = upper_apportionment(vm+0.1, seats)
    expect_true(is.matrix(lower_apportionment(vm+0.1, ua$district, ua$party)))
    expect_error_fixed(lower_apportionment(vm+0.1, seats, 1:3), "sum(seats_cols) == sum(seats_rows")

    # votes_matrix
    vm_names = matrix(1, 3, 2)
    rownames(vm_names) <- c("A", "A", "B")
    expect_error_fixed(prep_votes_matrix(vm_names, "x"), "rownames in `x` must be unique")
    colnames(vm_names) <- c("I", "I")
    expect_error_fixed(prep_votes_matrix(vm_names, "x"), "rownames in `x` must be unique")
    rownames(vm_names) <- c("A", "C", "B")
    expect_error_fixed(prep_votes_matrix(vm_names, "x"), "colnames in `x` must be unique")

    # max iterations
    options(proporz_max_iterations = 2)
    expect_error_fixed(biproporz(vm, seats), "Result is undefined, exceeded maximum number of iterations (2)")
    options(proporz_max_iterations = NULL)
})

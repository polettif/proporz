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

    x3 = lower_apportionment(M2, d2, p2, method = function(x) ceil_at(x, 0.5))
    expect_equal(x3, x2)

    expect_warning(
        lower_apportionment(matrix(c(1,0,1,0), 2), c(1,1), c(2,0), method = "harmonic"),
        'Lower apportionment is only guaranteed to terminate with the default Sainte-Lagu\uEB/Webster method (method = "round")',
        fixed = TRUE)
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
    vm = vm0
    vm[c(1,3),] <- round(c(7.97, 5.14)*vm[c(1,3),])

    q1 = apply_quorum(vm, quorum_any(any_district = 35))
    check1 = all(q1[2,] == c(0,0))
    q2 = apply_quorum(vm, quorum_any(total = 45))
    check2 = all(q2[2,] == c(0,0))
    q3 = apply_quorum(vm, quorum_all(any_district = 35, total = 45))
    check3 = identical(q2, q3)
    expect_true(all(check1, check2, check3))

    expect_equal(sum(apply_quorum(vm, c(F,F,F))), 0)
    expect_equal(sum(apply_quorum(vm, c(F,T,F))), 30)
    expect_error(apply_quorum(vm, "x"), "Cannot parse quorum function or vector")
})

test_that("quorum with percentages counts", {
    vm = matrix(c(30, 10, 60, 50, 20, 180), nrow = 3)
    p1 = apply_quorum(vm, quorum_any(any_district = 0.15))
    expect_equal(p1[2,], c(0,0))
    p2 = apply_quorum(vm, quorum_any(any_district = 0.09))
    expect_true(all(p2[2,] != c(0,0)))
    p3 = apply_quorum(vm, quorum_any(total = 0.085))
    expect_equal(p2,p3)
    p4 = apply_quorum(vm, quorum_any(total = 0.09))
    expect_equal(p1, p4)
})

test_that("quorum", {
    vm = matrix(c(90, 4, 5, 1, 104, 4, 1, 1), ncol = 2)*10

    q_district = c(T,F,T,F)
    q_total = c(T,T,F,F)
    q_district_and_total = c(T,F,F,F)
    q_district_or_total = c(T,T,T,F)

    # reached_quorums
    expect_equal(reached_quorums(vm, quorum_any(any_district = 0.05)),
                 q_district)
    expect_equal(reached_quorums(vm, quorum_all(any_district = 0.05)),
                 q_district)
    expect_equal(reached_quorums(vm, quorum_any(total = 0.03)),
                 q_total)
    expect_equal(reached_quorums(vm, quorum_all(total = 0.03)),
                 q_total)
    expect_equal(reached_quorums(vm, quorum_any(any_district = 0.05, total = 0.03)),
                 q_district_or_total)
    expect_equal(reached_quorums(vm, quorum_all(any_district = 0.05, total = 0.03)),
                 q_district_and_total)
    expect_equal(reached_quorums(vm, quorum_all(any_district = 0.05, total = 63)),
                 q_district_and_total)
    expect_error(reached_quorums(vm, reached_quorum_total), "reached_quorum_total is not a list of functions")
    expect_error(reached_quorums(vm, list(1, 2)), ".* is not a list of functions")

    dummy_list = list(function(x) x > 0, function(x) x < 0)
    expect_error(reached_quorums(vm, dummy_list), "type must be set as list attribute")
    attributes(dummy_list)$type <- "either_or"
    expect_error(reached_quorums(vm, dummy_list), "unknown type either_or")

    # vote_matrix
    soll_district = vm * matrix(rep(q_district, 2), ncol = 2)
    soll_total = vm * matrix(rep(q_total, 2), ncol = 2)
    soll_district_and_total = vm * matrix(rep(q_district_and_total, 2), ncol = 2)
    soll_district_or_total = vm * matrix(rep(q_district_or_total, 2), ncol = 2)

    # biproportional()
    expect_equal(
        biproportional(vm, c(100, 100), quorum_any(any_district = 0.05)) > 0,
        soll_district > 0)
    expect_equal(
        biproportional(vm, c(100, 100), quorum_any(any_district = 0.05)) > 0,
        soll_district > 0)

    expect_equal(
        biproportional(vm, c(100, 100), quorum_any(total = 0.03)) > 0,
        soll_total > 0)
    expect_equal(
        biproportional(vm, c(100, 100), quorum_all(any_district = 0.05, total = 0.03)) > 0,
        soll_district_and_total > 0)
    expect_equal(
        biproportional(vm, c(100, 100), quorum_any(any_district = 0.05, total = 0.03)) > 0,
        soll_district_or_total > 0)

    # with vector
    expect_equal(
        biproportional(vm, c(100, 100), q_district_or_total) > 0,
        soll_district_or_total > 0)

    # pukelsheim()
    vm_df = pivot_to_df(vm)
    wrap_pukelsheim = function(...) {
        seats_df = data.frame(col = c(1,2), seats = c(100, 100))
        unname(pivot_to_matrix(pukelsheim(vm_df, seats_df, ...)[c(1,2,4)]))
    }

    expect_equal(
        wrap_pukelsheim(quorum = quorum_any(any_district = 0.05)) > 0,
        soll_district > 0)
    expect_equal(
        wrap_pukelsheim(quorum = quorum_any(total = 0.03)) > 0,
        soll_total > 0)
    expect_equal(
        wrap_pukelsheim(quorum = quorum_all(any_district = 0.05, total = 0.03)) > 0,
        soll_district_and_total > 0)
    expect_equal(
        wrap_pukelsheim(quorum = quorum_any(any_district = 0.05, total = 0.03)) > 0,
        soll_district_or_total > 0)

    # biproportional quorum edge case
    ec_vm = matrix(c(89,5,2,4, 96,1,1,4), ncol = 2)
    ec_bp = biproportional(vm, c(93, 100), quorum_any(any_district = 0.05))
    expect_is(ec_bp, "proporz_matrix")
})


test_that("free apportionment for districts", {
    # https://en.wikipedia.org/wiki/Biproportional_apportionment#Specific_example
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

    x = pklshm[,2:3]
    expect_error(
        pukelsheim(x, pklshm_seats),
        "x must be a data frame with 3 columns in the following order:\nparty, district and votes (names can differ)",
        fixed = T)

    x = pklshm[,c(2,1,3)]
    expect_error(pukelsheim(x, pklshm_seats),
                 "District ids not found in 2nd column. Are x's columns in the correct order (party, district, votes)?",
                 fixed = T)

    result = pukelsheim(pklshm, pklshm_seats, new_seats_col = "Sitze")
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

test_that("almost empty vote_matrix", {
    vm1 = matrix(c(10,0,0,0, 0,0,10,0, 0,0,0,20, 0,0,0,20), nrow = 4)
    expect_equal(biproportional(vm1, 4), biproportional(vm1, rep(1,4)))

    vm2 = matrix(0, nrow = 4, ncol = 4)
    vm2[1,2] <- 10
    expect_equal(sum(biproportional(vm2, 2)), 2)

    expect_error(biproportional(vm2, c(1,1,0,0)), "No votes in a district with at least one seat")

    vm3 = matrix(c(4,3,0,20,1,0), nrow = 2)
    expect_error(
        biproporz(vm3, c(1,3,4)),
        "Result is undefined, cannot assign all seats in lower apportionment")
})

test_that("undefined result biproportional", {
    seats = c(10, 20, 1, 1)
    set.seed(1284)
    vm = matrix(runif(4*10), ncol = 4) * matrix(rep(seats, 10), byrow = T, ncol = 4) * 1000
    vm <- round(vm)
    vm[vm < 200] <- 0

    expect_equal(upper_apportionment(vm, seats, use_list_votes = F)$party,
                 proporz(rowSums(vm), sum(seats), "round"))

    expect_error(upper_apportionment(vm, seats),
                 "Result is undefined, equal quotient for parties: 4 & 6",
                 fixed = T)

    # manual fix (actual implementation depends on rules)
    vm4 <- vm6 <- vm
    vm4[4,1] <- vm4[4,1]+1
    vm6[6,4] <- vm6[6,4]+1
    ua4 = upper_apportionment(vm4, seats)
    ua6 = upper_apportionment(vm6, seats)
    expect_equal(ua4$party[4], ua6$party[6])
})

test_that("find_divisor", {
    v = c(80,10,10)
    .check = function(div) round(v/div)

    d0 = find_divisor(v, 0, 100, 10)
    expect_equal(.check(d0), .check(10))
    # expand lower limit
    d1 = find_divisor(v, 20, 100, 10)
    expect_equal(.check(d1), .check(10))
    # expand upper limit
    d2 = find_divisor(v, 1, 5, 10)
    expect_equal(.check(d2), .check(10))
})

test_that("districts with one seat", {
    seats = c(10, 20, 1, 1)
    set.seed(80)

    votes_matrix = matrix(runif(4*10), ncol = 4) * matrix(rep(seats, 10), byrow = T, ncol = 4) * 100
    votes_matrix <- round(votes_matrix)
    votes_matrix[votes_matrix < 30] <- 0

    expect_equal(colSums(biproportional(votes_matrix, seats)), seats)
})

test_that("named votes_matrix", {
    votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)
    dimnames(votes_matrix) <- list(c("A", "B", "C", "D"), c("Z1", "Z2"))

    expect_error(biproportional(votes_matrix, c(50, 20)),
                 "needs to have the same names as the columns in votes_matrix")
})

test_that("error messages", {
    set.seed(2)
    vm = matrix(round(runif(12, 50, 1500)), nrow = 3)
    seats = c(10,20,10,9)

    # pukelsheim
    vdf = pivot_to_df(vm)
    colnames(vdf) <- c("party", "district", "votes")
    seats_df = data.frame(col = 1:4, seats = seats)
    seats_df124 = seats_df[-3,]
    vdf134 = vdf[vdf$col != 2,]

    # unique party ids
    vdf_dupl = rbind(vdf, vdf[9:12,])
    expect_error(pukelsheim(vdf_dupl, seats_df),
                 "There are duplicate party-district pairs in vdf_dupl")

    # unique district ids
    expect_error(pukelsheim(vdf_dupl, rbind(seats_df, seats_df)),
                 "District ids are not unique")

    # more helpful message if columns are switched
    expect_error(pukelsheim(vdf[,c(2,1,3)], seats_df),
                 "District ids not found in 2nd column. Are vdf[, c(2, 1, 3)]'s columns in the correct order (party, district, votes)?",
                 fixed = T)

    # districts not in vote_districts
    expect_error(pukelsheim(vdf134, seats_df),
                 "Not all district ids in seats_df's 1st column exist in vdf134's 2nd column")

    # vote_districts not in districts
    expect_error(pukelsheim(vdf, seats_df124),
                 "Not all district ids in vdf's 2nd column exist in seats_df124's 1st column")

    # mismatch on both ends
    expect_error(pukelsheim(vdf134, seats_df124),
                 "Not all district ids in seats_df124's 1st column exist in vdf134's 2nd column")

    # non-numeric
    vdf_non_num = vdf
    vdf_non_num$votes <- as.character(vdf_non_num$votes)
    expect_error(pukelsheim(vdf_non_num, seats_df),
                 "Vote values in vdf_non_num's third column must be numbers >= 0")

    # negative votes
    vdf_neg = vdf
    vdf_neg$votes <- vdf_neg$votes-500
    expect_error(pukelsheim(vdf_neg, seats_df),
                 "Vote values in vdf_neg's third column must be numbers >= 0")

    # biproportional
    expect_error(biproportional(vdf, c(1,2,3)), "vdf must be a matrix")
    expect_error(biproportional(vm, c(1,2,3)), "vm needs to have districts as columns and parties as rows")
    expect_error(biproportional(vm, seats, method = "quota_largest_remainder"), "Only divisor methods possible")
    expect_error(biproportional(vm+0.1, seats), "votes_matrix must only contain integers")
    expect_error(biproportional(vm, seats+0.1), "district_seats must be integers")
    expect_error(biproportional(vm, seats, method = c("round", "floor", "ceiling")),
                 "Only one or two methods allowed")

    # lower_apportionment
    expect_error(lower_apportionment(vm+0.1), "matrix must only contain integers")
})

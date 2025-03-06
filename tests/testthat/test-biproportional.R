expect_error_fixed = function(...) testthat::expect_error(..., fixed = TRUE)

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
    expect_error_fixed(weight_list_votes(vm, 1), "`length(district_seats)` must be the same as `ncol(votes_matrix)`")
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
    expect_false(is.null(get_divisors(result)$districts))
    expect_false(is.null(get_divisors(result)$parties))
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
                       "needs to have the same names as the columns in `unname(votes_matrix)`")
    expect_error_fixed(biproporz(votes_matrix, c(Z0 = 50, Z2 = 20)),
                       "needs to have the same names as the columns in `votes_matrix`")

    seats = c("Z2" = 20, "Z1" = 50)
    expect_identical(biproporz(votes_matrix, seats), biproporz(votes_matrix, seats[2:1]))
    expect_identical(biproporz(votes_matrix, seats)[,c("Z1", "Z2")],
                     biproporz(votes_matrix[,2:1], seats[2:1])[,c("Z1", "Z2")])
})

test_that("flow criterion helper", {
    M = matrix(c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE), byrow = TRUE, ncol = 3)
    expect_false(is_flow_criterion_pair(c(FALSE,FALSE),c(TRUE,FALSE)))
    expect_false(is_flow_criterion_pair(c(TRUE,FALSE),c(FALSE,FALSE)))
    expect_identical(
        apply(M, 1, is_flow_criterion_pair, M[1,]), c(TRUE, TRUE, TRUE, TRUE, FALSE))
    expect_identical(
        apply(M, 1, is_flow_criterion_pair, M[2,]), c(FALSE, TRUE, TRUE, FALSE, FALSE))
    expect_identical(
        apply(M, 1, is_flow_criterion_pair, M[5,]), c(FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("find_divisor", {
    v = matrix(c(80,10,10))
    .check = function(div) round(v/div)

    d0 = find_divisor(v, 0.1, 100, 10, function(x) ceil_at(x, 0.5))
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

expect_error_fixed = function(...) testthat::expect_error(..., fixed = TRUE)

test_that("undefined result biproportional", {
    seats = c(10, 20, 1, 1)
    set.seed(1284)
    vm = matrix(runif(4*10), ncol = 4) * matrix(rep(seats, 10), byrow = TRUE, ncol = 4) * 1000
    vm <- round(vm)
    vm[vm < 200] <- 0

    expect_identical(upper_apportionment(vm, seats, weight_votes = FALSE)$party,
                     proporz(rowSums(vm), sum(seats), "round"))

    expect_error_fixed(upper_apportionment(vm, seats),
                       "Result is undefined, equal quotient for parties: 4, 6")

    expect_error_fixed(biproporz(uri2020$votes_matrix, uri2020$seats_vector, quorum_any(any_district = 0.7)),
                       "Result is undefined, equal quotient for parties: 'CVP', 'SPGB', 'FDP', 'SVP'")

    vm5 = matrix(c(10, 10, 10, 10), 2, 2)
    expect_error_fixed(biproporz(vm5, c(3,1)),
                       "Result is undefined, tied votes and multiple possible seat assignments")

    vm0.5 = matrix(c(1500, 4500), 2)
    expect_identical(c(biproporz(vm0.5, 5, method = "round")), c(1L, 4L))
    expect_identical(c(biproporz(vm0.5, 7, method = "round")), c(2L, 5L))

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
    expect_error(pukelsheim(vdf, vdf_seats),
                 "Result is undefined, cannot assign all seats in lower apportionment")

    expect_error(
        biproporz(matrix(c(50,40,30,20,25,20,15,10), nrow = 4), c(10, 10)),
        "Result is undefined, cannot assign all seats in lower apportionment")

    expect_is(
        biproporz(matrix(c(10,15,10,15,10,10,10,10,15), 3), c(1,1,1)),
        "proporz_matrix")
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
        lower_apportionment(
            matrix(c(0,0,0,1,0,0,0,0,0),3), c(1,1,1), c(1,0,2)),
        "Not enough non-zero votes matrix entries to assign seats in districts: 1, 3")

    expect_error_fixed(
        lower_apportionment(
            matrix(c(1,0,0,1,0,0,0,0,0),3), c(1,1,0), c(1,1,0)),
        "Not enough non-zero votes matrix entries to assign seats to party: 2")

    expect_error_fixed(
        lower_apportionment(
            matrix(0, nrow = 4), c(1,1,1,1), c(1,0,1,2),
            "Not enough non-zero votes matrix entries to assign seats in districts: 1, 3, 4"))

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
    expect_error_fixed(
        biproporz(vm_blocks1, seats_blocks1),
        "Not enough seats for parties 'A', 'B' in districts 'District 1', 'District 2'\n(11 seats necessary, 10 available")

    vm_blocks2 = vm_blocks1[c(4,3,1,2),c(1,3,2,4)]
    dimnames(vm_blocks2) <- list(LETTERS[1:4], as.character(1:4))
    seats_blocks2 = setNames(seats_blocks1[c(1,3,2,4)], colnames(vm_blocks2))
    expect_error_fixed(
        biproporz(vm_blocks2, seats_blocks2),
        "Not enough seats for parties 'C', 'D' in districts '1', '3'\n(11 seats necessary, 10 available)")

    vm_blocks3 = uri2020$votes_matrix
    vm_blocks3[1,c(1,2,4)] <- vm_blocks3[4,c(1,2,4)] <- 0
    expect_error(biproporz(vm_blocks3, uri2020$seats_vector),
                 "Not enough seats for parties 'CVP', 'SVP' in district 'Erstfeld'")

    # no submatrix error for matrix with diag = 0
    vm_diag = matrix(c(0, 20, 100, 20, 0, 20, 100, 100, 0), nrow = 3)
    expect_is(biproporz(vm_diag, c(50, 30, 90)), "proporz_matrix")
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
    vdf134 = vdf[vdf[["district"]] != 2,]
    expect_gt(nrow(vdf134), 0)

    # unique party ids
    vdf_dupl = rbind(vdf, vdf[9:12,])
    expect_error_fixed(pukelsheim(vdf_dupl, seats_df),
                       "There are duplicate party-district pairs in `vdf_dupl`.")

    # unique district ids
    expect_error_fixed(pukelsheim(vdf_dupl, rbind(seats_df, seats_df)),
                       "District ids in `rbind(seats_df, seats_df)` are not unique")

    # more helpful message if columns are switched
    expect_error_fixed(pukelsheim(vdf[,c(2,1,3)], seats_df),
                       paste0("District ids not found in second column of `vdf[, c(2, 1, 3)]`. ",
                              "Are columns in the correct order (party, district, votes)?"))

    # districts not in vote_districts
    expect_error_fixed(pukelsheim(vdf134, seats_df),
                       paste0("Not all district ids in `seats_df`s first column exist ",
                              "in `vdf134`s second column"))

    # vote_districts not in districts
    expect_error_fixed(pukelsheim(vdf, seats_df124),
                       "Not all district ids in `vdf`s second column exist in `seats_df124`s first column")

    # mismatch on both ends
    expect_error_fixed(pukelsheim(vdf134, seats_df124),
                       "Not all district ids in `seats_df124`s first column exist in `vdf134`s second column")

    # seats_df is not a data.frame
    seats_vec124 = setNames(seats_df124$seats, seats_df124$col)
    expect_error_fixed(pukelsheim(vdf134, seats_vec124), "`seats_vec124` must be a data.frame")

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
    expect_error_fixed(biproporz(vm, NA), "`NA` must be a numeric vector, data.frame or a single number")
    expect_error_fixed(biproporz(vdf, c(1,2,3)), "`vdf` must be a matrix")
    expect_error_fixed(biproporz(vm, c(1,2,3)), "`vm` must have districts as columns and parties as rows")
    expect_error_fixed(biproporz(vm, seats, method = "largest_remainder_method"),
                       'Cannot use "largest_remainder_method", only divisor methods are possible in biproportional apportionment')
    expect_s3_class(biproporz(vm+0.1, seats), "proporz_matrix")
    expect_s3_class(biproporz(vm*0.1, seats), "proporz_matrix")
    expect_identical(as.matrix(biproporz(vm*0.01, seats)), as.matrix(biproporz(vm*20, seats)))
    expect_error_fixed(biproporz(vm, seats+0.1), "`seats + 0.1` must be integers")
    expect_error_fixed(biproporz(vm, seats, method = c("round", "floor", "ceiling")),
                       "Only one or two methods allowed")
    expect_error_fixed(biproporz(vm, seats, method = round),
                       "Method must be a character or a list")
    expect_error_fixed(biproporz(vm, vm),
                       "`vm` must be a numeric vector, data.frame or a single number")

    # upper/lower_apportionment
    ua = upper_apportionment(vm+0.1, seats)
    expect_true(is.matrix(lower_apportionment(vm+0.1, ua$district, ua$party)))
    expect_error_fixed(lower_apportionment(vm+0.1, seats, 1:3), "sum(seats_cols) == sum(seats_rows")

    # max iterations
    options(proporz_max_iterations = 2)
    expect_error_fixed(biproporz(vm, seats), "Result is undefined, exceeded maximum number of iterations (2)")
    options(proporz_max_iterations = NULL)

    # custom function
    expect_error(
        lower_apportionment(matrix(c(21,11,33,21), 2), c(2,2), c(2,2), method = function(x) x),
        "Rounding function does not return integers")
})

test_that("unique name checks", {
    # votes_matrix
    vm_names = matrix(1, 3, 2)
    rownames(vm_names) <- c("A", "A", "B")
    expect_error_fixed(prep_votes_matrix(vm_names, "x"), "rownames in `x` must be unique without NA's")
    colnames(vm_names) <- c("I", "I")
    expect_error_fixed(prep_votes_matrix(vm_names, "x"), "rownames in `x` must be unique without NA's")
    rownames(vm_names) <- c("A", "C", "B")
    expect_error_fixed(prep_votes_matrix(vm_names, "x"), "colnames in `x` must be unique without NA's")

    # district seats
    ds_dupl = c("I" = 5, "I" = 5)
    expect_error_fixed(prep_district_seats(ds_dupl, vm_names, "distrdupl", "xy"),
                       "`distrdupl` must have unique names without NA's")
    names(ds_dupl)[2] <- NA
    colnames(vm_names)[2] <- NA
    expect_error_fixed(prep_district_seats(ds_dupl, vm_names, "distrdupl", "xy"),
                       "`distrdupl` must have unique names without NA's")
})

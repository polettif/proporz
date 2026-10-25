# proportional ####
test_that("quorum proporz", {
    method_list = unique(unlist(proporz_methods, use.names = FALSE))

    for(method in method_list) {
        expect_error_fixed(proporz(c(50, 30), 3, method, 60), "No party reached the quorum")
    }
})

test_that("apply_quorum_boolvector", {
    votes = c(49, 38, 13)
    expect_equal(apply_quorum_boolvector(votes, 0), c(49, 38, 13), tolerance = 1e-14)
    expect_equal(apply_quorum_boolvector(votes, 0.13), c(49, 38, 13), tolerance = 1e-14)
    expect_equal(apply_quorum_boolvector(votes, 0.135), c(49, 38, 0), tolerance = 1e-14)
    expect_equal(apply_quorum_boolvector(votes, 15), c(49, 38, 0), tolerance = 1e-14)
})

test_that("check seats number after quorum", {
    v = c(100, 1, 1)
    expect_error(divisor_ceiling(v, 1), "at least as many seats")
    expect_error(divisor_geometric(v, 1), "at least as many seats")
    expect_error(divisor_harmonic(v, 1), "at least as many seats")

    expect_no_error(divisor_ceiling(v, 1, 2))
    expect_no_error(divisor_geometric(v, 1, 2))
    expect_no_error(divisor_harmonic(v, 1, 2))
})

# biproportional ####
test_that("quorum with vote counts", {
    vm0 = matrix(c(30, 10, 60, 50, 20, 180), nrow = 3)
    vm = vm0
    vm[c(1,3),] <- round(c(7.97, 5.14)*vm[c(1,3),])

    q1 = eval_quorum(quorum_all(votes_any_district = 35), vm)
    expect_identical(q1, c(TRUE, FALSE, TRUE))
    q2 = eval_quorum(quorum_all(votes_total = 45), vm)
    expect_identical(q2, c(TRUE, FALSE, TRUE))
    q3 = eval_quorum(quorum_all(votes_any_district = 35, votes_total = 45), vm)
    expect_identical(q3, c(TRUE, FALSE, TRUE))

    expect_identical(reached_quorum_any_district(vm, 35), q1)
    expect_identical(reached_quorum_total(vm, 45), q2)

    expect_identical(sum(zero_failed_quorums(vm, c(FALSE,FALSE,FALSE))), 0)
    expect_identical(sum(zero_failed_quorums(vm, c(FALSE,TRUE,FALSE))), 30)
})

test_that("quorum with percentages counts", {
    vm = matrix(c(30, 10, 60, 50, 20, 180), nrow = 3)
    p1 = eval_quorum(quorum_all(votes_any_district = 0.15), vm)
    expect_identical(p1, c(TRUE, FALSE, TRUE))
    p2 = eval_quorum(quorum_all(votes_any_district = 0.09), vm)
    expect_identical(p2, c(TRUE, TRUE, TRUE))
    p3 = eval_quorum(quorum_all(votes_total = 0.085), vm)
    expect_identical(p3, c(TRUE, TRUE, TRUE))
    p4 = eval_quorum(quorum_all(votes_total = 0.09), vm)
    expect_identical(p1, p4)

    vm2 = matrix(c(30, 10, 60, 50, 20, 180), nrow = 6)
    expected = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
})

test_that("eval_quorum", {
    vm = matrix(c(90, 4, 5, 1, 104, 4, 1, 1), ncol = 2)*10

    q_district = c(TRUE,FALSE,TRUE,FALSE)
    q_total = c(TRUE,TRUE,FALSE,FALSE)
    q_district_and_total = c(TRUE,FALSE,FALSE,FALSE)
    q_district_or_total = c(TRUE,TRUE,TRUE,FALSE)

    expect_identical(
        eval_quorum(quorum_any(votes_any_district = 0.05), vm),
        q_district)
    expect_identical(
        eval_quorum(quorum_all(votes_any_district = 0.05), vm),
        q_district)
    expect_identical(
        eval_quorum(quorum_any(votes_total = 0.03), vm),
        q_total)
    expect_identical(
        eval_quorum(quorum_all(votes_total = 0.03), vm),
        q_total)
    expect_identical(
        eval_quorum(quorum_any(votes_any_district = 0.05, votes_total = 0.03), vm),
        q_district_or_total)
    expect_identical(
        eval_quorum(quorum_all(votes_any_district = 0.05, votes_total = 0.03), vm),
        q_district_and_total)
    expect_identical(
        eval_quorum(quorum_all(votes_any_district = 0.05, votes_total = 63), vm),
        q_district_and_total)

    # weighted_votes
    expect_identical(
        eval_quorum(quorum_any(any_district_voters = 0.05), weighted_votes_matrix = vm),
        q_district)
    expect_identical(
        eval_quorum(quorum_all(any_district_voters = 0.05), weighted_votes_matrix = vm),
        q_district)
    expect_identical(
        eval_quorum(quorum_any(total_voters = 0.03), weighted_votes_matrix = vm),
        q_total)
    expect_identical(
        eval_quorum(quorum_all(total_voters = 0.03), weighted_votes_matrix = vm),
        q_total)

    # errors
    expect_error_fixed(
        eval_quorum(quorum_all(votes_total = 0.03), weighted_votes_matrix = vm),
        "Argument `votes_matrix` is missing (needed for `votes_total`)")
    expect_error_fixed(
        eval_quorum(quorum_all(votes_any_district = 0.03), weighted_votes_matrix = vm),
        "Argument `votes_matrix` is missing (needed for `votes_any_district`)")
    expect_error_fixed(
        eval_quorum(quorum_all(total_voters = 0.03), votes_matrix = vm),
        "Argument `weighted_votes_matrix` is missing (needed for `total_voters`)")
    expect_error_fixed(
        eval_quorum(quorum_all(any_district_voters = 0.03), votes_matrix = vm),
        "Argument `weighted_votes_matrix` is missing (needed for `any_district_voters`)")
    expect_error_fixed(
        eval_quorum(reached_quorum_total, vm),
        "Quorum must be a quorum parameter list or a logical vector, see ?quorum_functions")
    expect_error_fixed(
        eval_quorum(list(1, 2), vm),
        "Quorum must be a quorum parameter list or a logical vector, see ?quorum_functions")

    dummy_list = list(function(x) x > 0, function(x) x < 0)
    expect_error_fixed(eval_quorum(dummy_list, vm), "Quorum must be a quorum parameter list or a logical vector, see ?quorum_functions")
    attributes(dummy_list)$type <- "either_or"
    expect_error_fixed(eval_quorum(dummy_list, vm), "Quorum must be a quorum parameter list or a logical vector, see ?quorum_functions")
    attributes(dummy_list)$type <- c("ALL", "ANY")
    expect_error_fixed(eval_quorum(dummy_list, vm), "Quorum must be a quorum parameter list or a logical vector, see ?quorum_functions")
    # TODO change names/doc for reached_quorum to eval_quorum
})

test_that("quorum_all/quorum_any in biproporz", {
    vm = matrix(c(90, 4, 5, 1, 104, 4, 1, 1), ncol = 2)*10

    q_district = c(TRUE,FALSE,TRUE,FALSE)
    q_total = c(TRUE,TRUE,FALSE,FALSE)
    q_district_and_total = c(TRUE,FALSE,FALSE,FALSE)
    q_district_or_total = c(TRUE,TRUE,TRUE,FALSE)

    # vote_matrix
    soll_district = vm * matrix(rep(q_district, 2), ncol = 2)
    soll_total = vm * matrix(rep(q_total, 2), ncol = 2)
    soll_district_and_total = vm * matrix(rep(q_district_and_total, 2), ncol = 2)
    soll_district_or_total = vm * matrix(rep(q_district_or_total, 2), ncol = 2)

    # biproporz
    expect_identical(
        biproporz(vm, c(100, 100), quorum_any(votes_any_district = 0.05)) > 0,
        soll_district > 0)
    expect_identical(
        biproporz(vm, c(100, 100), quorum_any(votes_any_district = 0.05)) > 0,
        soll_district > 0)
    expect_identical(
        biproporz(vm, c(100, 100), quorum_any(votes_total = 0.03)) > 0,
        soll_total > 0)
    expect_identical(
        biproporz(vm, c(100, 100), quorum_all(votes_any_district = 0.05, votes_total = 0.03)) > 0,
        soll_district_and_total > 0)
    expect_identical(
        biproporz(vm, c(100, 100), quorum_any(votes_any_district = 0.05, votes_total = 0.03)) > 0,
        soll_district_or_total > 0)

    # with vector
    expect_identical(
        biproporz(vm, c(100, 100), q_district_or_total) > 0,
        soll_district_or_total > 0)

    # pukelsheim
    vm_df = pivot_to_df(vm)
    wrap_pukelsheim = function(...) {
        seats_df = data.frame(col = c(1,2), seats = c(100, 100))
        unname(pivot_to_matrix(pukelsheim(vm_df, seats_df, ...)[c(1,2,4)]))
    }

    expect_identical(
        wrap_pukelsheim(quorum = quorum_any(votes_any_district = 0.05)) > 0,
        soll_district > 0)
    expect_identical(
        wrap_pukelsheim(quorum = quorum_any(votes_total = 0.03)) > 0,
        soll_total > 0)
    expect_identical(
        wrap_pukelsheim(quorum = quorum_all(votes_any_district = 0.05, votes_total = 0.03)) > 0,
        soll_district_and_total > 0)
    expect_identical(
        wrap_pukelsheim(quorum = quorum_any(votes_any_district = 0.05, votes_total = 0.03)) > 0,
        soll_district_or_total > 0)

    # biproportional quorum edge case
    ec_vm = matrix(c(89,5,2,4, 96,1,1,4), ncol = 2)
    ec_bp = biproporz(vm, c(93, 100), quorum_any(votes_any_district = 0.05))
    expect_is(ec_bp, "proporz_matrix")
})

test_that("apply_quorum", {
    vc = c(91, 9)
    expect_identical(apply_quorum(vc, 0), apply_quorum_boolvector(vc, 0))
    expect_identical(apply_quorum(vc, 0.1), apply_quorum_boolvector(vc, 0.1))
    expect_identical(apply_quorum(vc, 10), apply_quorum_boolvector(vc, 10))

    vm = matrix(c(89, 11, 199, 1), 2)
    expect_identical(rowSums(apply_quorum(vm, quorum_any(votes_any_district = 0.1))) > 0,
                     eval_quorum(quorum_any(votes_any_district = 0.1), vm))
    expect_identical(rowSums(apply_quorum(vm, quorum_all(votes_any_district = 0.1, votes_total = 0.05))) > 0,
                     eval_quorum(quorum_all(votes_any_district = 0.1, votes_total = 0.05), vm))

    expect_error_fixed(
        apply_quorum(vm, 0),
        "Quorum must be a quorum parameter list or a logical vector, see ?quorum_functions")
    expect_identical(sum(apply_quorum(vm, c(FALSE, FALSE))), 0)
    expect_error_fixed(
        apply_quorum(vm, c(FALSE)),
        "length(quorum) == nrow(votes_matrix) is not TRUE")

    expect_error_fixed(
        apply_quorum(vc, quorum_all(0.5)),
        "Quorum parameter must be a single number >= 0")
    expect_error_fixed(
        apply_quorum(vc, -1),
        "Quorum parameter must be a single number >= 0")

    expect_error_fixed(
        apply_quorum(vm, quorum_all(total_voters = 0.1)),
        "Cannot handle unweighted matrix with method: total_voters")
    expect_error_fixed(
        apply_quorum(vm, quorum_all(any_district_voters = 0.1)),
        "Cannot handle unweighted matrix with method: any_district_voters")
    expect_error_fixed(
        apply_quorum(vm, quorum_all(any_district_voters = 0.1, total_voters = 10)),
        "Cannot handle unweighted matrix with methods: any_district_voters, total_voters")

})

test_that("quorum vector in biproporz", {
    votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)
    dimnames(votes_matrix) <- list(party = c("A", "B", "C", "D"), district = c("Z1", "Z2"))
    seats = c(Z1 = 50, Z2 = 20)

    quorum_vec1 = c(A = FALSE, B = TRUE, C = TRUE, D = FALSE)
    bp = biproporz(votes_matrix, seats, quorum = quorum_vec1)
    expect_identical(rowSums(bp) > 0, quorum_vec1)
    quorum_vec2 = setNames(c(TRUE, TRUE, FALSE, FALSE), c("C", "B", "A", "D"))
    expect_identical(biproporz(votes_matrix, seats, quorum = quorum_vec2), bp)
    quorum_vec3 = unname(quorum_vec1)
    expect_error(
        biproporz(votes_matrix, seats, quorum = quorum_vec3),
        "quorum vector must have the same names as the votes_matrix rows")

    votes_df = pivot_to_df(votes_matrix)
    seats_df = data.frame(district = names(seats), seats = seats)
    pk = pukelsheim(votes_df, seats_df, quorum = quorum_vec2)
    expect_identical(sum(pk[pk$party %in% c("A", "D"), "seats"]), 0L)
    expect_error(
        pukelsheim(votes_df, seats_df, quorum = quorum_vec3),
        "quorum vector must have the same names as the votes_matrix rows")

    expect_error(quorum_all(), "No quorum argument given")
    expect_error(quorum_any(), "No quorum argument given")
    expect_error_fixed(upper_apportionment(votes_matrix, seats, quorum = list()),
                       "Quorum must be a quorum parameter list or a logical vector, see ?quorum_functions")
    quorumlist = quorum_all(votes_any_district = 0.05)
    quorumlist$new_method <- 0.05
    expect_error_fixed(
        upper_apportionment(votes_matrix, seats, quorum = quorumlist),
        "Quorum must be a quorum parameter list or a logical vector, see ?quorum_functions")
})

test_that("renamend quorum parameters", {
    expect_error_fixed(
        quorum_all(total = 1),
        "The argument `total` is deprecated, please specify total_voters (recommended) or `votes_total`")

    expect_error_fixed(
        quorum_all(any_district = 1),
        "The argument `any_district` is deprecated, please specify `votes_any_district` (recommended) or `any_district_voters`")

    vm = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)  * 10
    expect_error(
        apply_quorum(vm, quorum_all(total_voters = 10)),
        "Cannot handle unweighted matrix with method: total_voters")
})

test_that("is_quorum_paramlist", {
    expect_false(is_quorum_paramlist(list()))
    expect_false(is_quorum_paramlist(list("votes2_total" = NA)))
    expect_false(is_quorum_paramlist(list("votes_total" = data.frame())))
    expect_false(is_quorum_paramlist(data.frame()))
})

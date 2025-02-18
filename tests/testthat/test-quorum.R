expect_error_fixed = function(...) testthat::expect_error(..., fixed = TRUE)

# proportional ####
test_that("quorum proporz", {
    method_list = unique(unlist(proporz_methods, use.names = FALSE))

    for(method in method_list) {
        expect_error(proporz(c(50, 30), 3, method, 60), "No party reached the quorum",
                     fixed = TRUE)
    }
})

test_that("apply_quorum_vector", {
    votes = c(49, 38, 13)
    expect_equal(apply_quorum_vector(votes, 0), c(49, 38, 13), tolerance = 1e-14)
    expect_equal(apply_quorum_vector(votes, 0.13), c(49, 38, 13), tolerance = 1e-14)
    expect_equal(apply_quorum_vector(votes, 0.135), c(49, 38, 0), tolerance = 1e-14)
    expect_equal(apply_quorum_vector(votes, 15), c(49, 38, 0), tolerance = 1e-14)
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
    expect_error_fixed(apply_quorum_matrix(vm, "x"),
                       "Quorum parameter must be a logical vector or a list of quorum functions (see ?quorum_functions)")
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
    expect_error_fixed(reached_quorums(vm, reached_quorum_total), "is_quorum_function_list(quorum_funcs) is not TRUE")
    expect_error_fixed(reached_quorums(vm, list(1, 2)), "is_quorum_function_list(quorum_funcs) is not TRUE")

    dummy_list = list(function(x) x > 0, function(x) x < 0)
    expect_error_fixed(reached_quorums(vm, dummy_list), "is_quorum_function_list(quorum_funcs) is not TRUE")
    attributes(dummy_list)$type <- "either_or"
    expect_error_fixed(reached_quorums(vm, dummy_list), "is_quorum_function_list(quorum_funcs) is not TRUE")
    attributes(dummy_list)$type <- c("ALL", "ANY")
    expect_error_fixed(reached_quorums(vm, dummy_list), "is_quorum_function_list(quorum_funcs) is not TRUE")

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

test_that("apply_quorum", {
    vc = c(91, 9)
    expect_identical(apply_quorum(vc, 0), apply_quorum_vector(vc, 0))
    expect_identical(apply_quorum(vc, 0.1), apply_quorum_vector(vc, 0.1))
    expect_identical(apply_quorum(vc, 10), apply_quorum_vector(vc, 10))

    vm = matrix(c(89, 11, 199, 1), 2)
    expect_identical(apply_quorum(vm, quorum_any(any_district = 0.1)),
                     apply_quorum_matrix(vm, quorum_any(any_district = 0.1)))
    expect_identical(apply_quorum(vm, quorum_all(any_district = 0.1, total = 0.05)),
                     apply_quorum_matrix(vm, quorum_all(any_district = 0.1, total = 0.05)))

    expect_error(apply_quorum(vm, 0),
                 "Quorum parameter must be a logical vector or a list of quorum functions (see ?quorum_functions)",
                 fixed = TRUE)
    expect_identical(sum(apply_quorum(vm, c(FALSE, FALSE))), 0)
    expect_error_fixed(apply_quorum(vm, c(FALSE)), "length(quorum) == nrow(votes_matrix) is not TRUE")

    expect_error(apply_quorum(vc, quorum_all(0.5)),
                 "Quorum parameter must be a single number >= 0", fixed = TRUE)
    expect_error(apply_quorum(vc, -1),
                 "Quorum parameter must be a single number >= 0", fixed = TRUE)
})

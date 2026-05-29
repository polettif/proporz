# based on fuzzr package
fuzz_params = readRDS(test_path("test-fuzzy.rds"))

fuzzy_errors = function(fun) {
    msg = lapply(fuzz_params,
                 function(param) {
                     tryCatch({
                         fun(param)
                         return(NULL)
                     },
                     error = function(e) {
                         if(startsWith(e$message, "missing value where TRUE/FALSE needed")) browser()
                         if(startsWith(e$message, "argument is of length zero")) browser()
                         if(startsWith(e$message, "invalid 'times' argument")) browser()
                         if(startsWith(e$message, "character string is not in a standard")) browser()

                         e$message
                     })
                 })
    unique(unlist(msg))
}

test_that("fuzzy params proporz", {
    # proporz
    votes = c(10, 30, 40)
    n_seats = 5
    method = "round"
    quorum = 0

    em = fuzzy_errors(function(votes) proporz(votes, n_seats, method, quorum))
    expect_identical(em, c("`votes` must be a numeric vector >= 0"))

    em = fuzzy_errors(function(n_seats) proporz(votes, n_seats, method, quorum))
    expect_identical(em, "`n_seats` must be an integer >= 0")

    em = fuzzy_errors(function(method) proporz(votes, n_seats, method, quorum))
    expect_identical(
        unique(sapply(strsplit(em, ": "), getElement, 1)),
        c("is.character(method_name) && length(method_name) == 1 is not TRUE",
          "Unknown apportion method"))

    em = fuzzy_errors(function(quorum) proporz(votes, n_seats, method, quorum))
    expect_identical(em, "Quorum parameter must be a single number >= 0")

    # highest_averages
    divisors = 0.3
    em = fuzzy_errors(function(votes) highest_averages_method(votes, n_seats, divisors))
    expect_identical(em, "`votes` must be a numeric vector >= 0")
    em = fuzzy_errors(function(n_seats) highest_averages_method(votes, n_seats, divisors))
    expect_identical(em, "`n_seats` must be an integer >= 0")
    em = fuzzy_errors(function(divisors) highest_averages_method(votes, n_seats, divisors))
    expect_identical(
        em,
        c("is.numeric(divisors) is not TRUE", "divisors >= 0 is not TRUE",
          "Number of divisors is not equal to the number of seats", "is.null(dim(divisors)) is not TRUE"
        ))

    # largest_remainder_method
    em = fuzzy_errors(function(votes) largest_remainder_method(votes, n_seats, quorum))
    expect_identical(em, "`votes` must be a numeric vector >= 0")
    em = fuzzy_errors(function(n_seats) largest_remainder_method(votes, n_seats, quorum))
    expect_identical(em, "`n_seats` must be an integer >= 0")
    em = fuzzy_errors(function(quorum) largest_remainder_method(votes, n_seats, quorum))
    expect_identical(em, "Quorum parameter must be a single number >= 0")
})

test_that("fuzzy params biproporz/pukelsheim", {
    # biproporz
    votes_matrix = uri2020$votes_matrix
    district_seats = uri2020$seats_vector
    quorum = NULL
    weight_votes = TRUE
    method = list("round", "round")

    em = fuzzy_errors(function(votes_matrix) biproporz(votes_matrix, district_seats, quorum, weight_votes, method))
    expect_identical(em, c("`votes_matrix` must be a matrix", "Votes in `votes_matrix` must be numbers >= 0",
                           "`district_seats` must have the same names as the columns in `votes_matrix`"))

    em = fuzzy_errors(function(district_seats) biproporz(votes_matrix, district_seats, quorum, weight_votes, method))
    expect_true(all(endsWith(em, "numeric vector, data.frame or a single number") |
                        endsWith(em, "must be integers") |
                        endsWith(em, "districts as columns and parties as rows") |
                        em %in% c("length(district_seats) == ncol(votes_matrix) is not TRUE", "is.atomic(district_seats) is not TRUE")))

    em = fuzzy_errors(function(quorum) biproporz(votes_matrix, district_seats, quorum, weight_votes, method))
    expect_identical(
        em,
        c(
            "Quorum parameter must be a logical vector or a list of quorum functions (see ?quorum_functions)",
            "length(quorum) == nrow(votes_matrix) is not TRUE"
        ))

    em = fuzzy_errors(function(weight_votes) biproporz(votes_matrix, district_seats, quorum, weight_votes, method))
    expect_identical(em, "`weight_votes` must be TRUE or FALSE")

    em = fuzzy_errors(function(method) biproporz(votes_matrix, district_seats, quorum, weight_votes, method))
    expect_true(all(em %in% c("is.character(method_name) && length(method_name) == 1 is not TRUE",
                              "Method must be a single character or a list of two characters",
                              "Only one or two methods allowed") | startsWith(em, "Unknown apportion method:")))

    # pukelsheim
    votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
    district_seats_df = unique(zug2018[c("entity_id", "election_mandates")])
    quorum = NULL
    new_seats_col = "seats"
    weight_votes = TRUE
    winner_take_one = FALSE

    em = fuzzy_errors(function(votes_df) pukelsheim(votes_df, district_seats_df, quorum, new_seats_col, weight_votes, winner_take_one))
    expect_true(startsWith(em, "`votes_df` must be a data frame with 3 columns in the following order"))

    em = fuzzy_errors(function(district_seats_df) pukelsheim(votes_df, district_seats_df, quorum, new_seats_col, weight_votes, winner_take_one))
    expect_identical(
        em,
        c("`district_seats_df` must be a data.frame", "District ids in `district_seats_df` are not unique",
          "Not all district ids in `district_seats_df`s first column exist in `votes_df`s second column"))

    em = fuzzy_errors(function(quorum) pukelsheim(votes_df, district_seats_df, quorum, new_seats_col, weight_votes, winner_take_one))
    expect_identical(
        em,
        c("Quorum parameter must be a logical vector or a list of quorum functions (see ?quorum_functions)",
          "length(quorum) == nrow(votes_matrix) is not TRUE"))

    em = fuzzy_errors(function(new_seats_col) pukelsheim(votes_df, district_seats_df, quorum, new_seats_col, weight_votes, winner_take_one))
    expect_identical(
        em,
        "is.character(new_seats_col) && length(new_seats_col) == 1 && !is.na(new_seats_col) is not TRUE")

    em = fuzzy_errors(function(weight_votes) pukelsheim(votes_df, district_seats_df, quorum, new_seats_col, weight_votes, winner_take_one))
    expect_identical(
        em,
        "is.logical(weight_votes) && length(weight_votes) == 1 && !is.na(weight_votes) is not TRUE")

    em = fuzzy_errors(function(winner_take_one) pukelsheim(votes_df, district_seats_df, quorum, new_seats_col, weight_votes, winner_take_one))
    expect_identical(
        em,
        "is.logical(winner_take_one) && length(winner_take_one) == 1 && !is.na(winner_take_one) is not TRUE")

    # upper_apportionment
    votes_matrix = matrix(c(123, 912, 312, 45, 714, 255, 815, 414, 215), nrow = 3)
    district_seats = c(7, 5, 8)
    weight_votes = TRUE
    method = "round"

    em = fuzzy_errors(function(votes_matrix) upper_apportionment(votes_matrix, district_seats, weight_votes, method))
    expect_identical(em, c("`votes_matrix` must be a matrix", "Votes in `votes_matrix` must be numbers >= 0",
                           "`votes_matrix` must have districts as columns and parties as rows"
    ))

    em = fuzzy_errors(function(district_seats) upper_apportionment(votes_matrix, district_seats, weight_votes, method))
    expect_identical(
        em,
        c("`district_seats` must be a numeric vector, data.frame or a single number",
          "length(district_seats) == ncol(votes_matrix) is not TRUE", "`district_seats` must be integers",
          "`votes_matrix` must have districts as columns and parties as rows",
          "is.atomic(district_seats) is not TRUE"))

    em = fuzzy_errors(function(weight_votes) upper_apportionment(votes_matrix, district_seats, weight_votes, method))
    expect_identical(em, "`weight_votes` must be TRUE or FALSE")

    em = fuzzy_errors(function(method) upper_apportionment(votes_matrix, district_seats, weight_votes, method))
    expect_true(all(startsWith(em, "is.character(method") | startsWith(em, "Unknown apportion method: ")))

    # lower_apportionment
    votes_matrix = matrix(c(123, 912, 312, 45, 714, 255, 815, 414, 215), nrow = 3)
    district_seats = c(7, 5, 8)
    party_seats = c(5, 11, 4)
    method = "round"

    em = fuzzy_errors(function(votes_matrix) lower_apportionment(votes_matrix, district_seats, party_seats, method))
    expect_identical(
        em,
        c("`votes_matrix` must be a matrix", "Votes in `votes_matrix` must be numbers >= 0",
          "length(seats_cols) == ncol(votes_matrix) is not TRUE"))

    em = fuzzy_errors(function(district_seats) lower_apportionment(votes_matrix, district_seats, party_seats, method))
    expect_identical(
        em,
        c("is.numeric(seats_cols) && is.atomic(seats_cols) is not TRUE",
          "length(seats_cols) == ncol(votes_matrix) is not TRUE", "sum(seats_cols) == sum(seats_rows) is not TRUE",
          "all((seats_cols%%1) == 0) is not TRUE"))

    em = fuzzy_errors(function(party_seats) lower_apportionment(votes_matrix, district_seats, party_seats, method))
    expect_identical(
        em,
        c("is.numeric(seats_rows) && is.atomic(seats_rows) is not TRUE",
          "length(seats_rows) == nrow(votes_matrix) is not TRUE", "sum(seats_cols) == sum(seats_rows) is not TRUE",
          "all((seats_rows%%1) == 0) is not TRUE"))

    em = fuzzy_errors(function(method) lower_apportionment(votes_matrix, district_seats, party_seats, method))
    expect_true(all(startsWith(em, "is.function(method) ") | startsWith(em, "Unknown apportion method: ")))
})

test_that("fuzzy params quorum", {
    # quorum_all/any
    any_district = 0
    total = 0

    expect_identical(
        fuzzy_errors(function(any_district) quorum_all(any_district, total)),
        "length(any_district) == 1 && is.numeric(any_district) && !is.na(any_district) is not TRUE")
    expect_identical(
        fuzzy_errors(function(any_district) quorum_any(any_district, total)),
        "length(any_district) == 1 && is.numeric(any_district) && !is.na(any_district) is not TRUE")
    expect_identical(
        fuzzy_errors(function(total) quorum_all(any_district, total)),
        "length(total) == 1 && is.numeric(total) && !is.na(total) is not TRUE")
    expect_identical(
        fuzzy_errors(function(total) quorum_any(any_district, total)),
        "length(total) == 1 && is.numeric(total) && !is.na(total) is not TRUE")

    # apply_quorum
    votes = c(81, 9, 10)
    quorum = 1

    em = fuzzy_errors(function(votes) apply_quorum(votes, quorum))
    expect_identical(
        em,
        c("`votes` must be a numeric vector >= 0", "is.matrix(votes) || is.atomic(votes) is not TRUE",
          "Quorum parameter must be a logical vector or a list of quorum functions (see ?quorum_functions)"))

    em = fuzzy_errors(function(quorum) apply_quorum(votes, quorum))
    expect_identical(em, "Quorum parameter must be a single number >= 0")

    votes_matrix = matrix(c(91, 9, 199, 1), nrow = 2)
    quorum = quorum_all(any_district = 0.1)

    em = fuzzy_errors(function(votes_matrix) apply_quorum(votes_matrix, quorum))
    expect_identical(
        em,
        c("`votes` must be a numeric vector >= 0", "Quorum parameter must be a single number >= 0",
          "is.matrix(votes) || is.atomic(votes) is not TRUE", "is.matrix(votes_matrix) && !anyNA(votes_matrix) is not TRUE"
        ))

    em = fuzzy_errors(function(quorum) apply_quorum(votes_matrix, quorum))
    expect_identical(
        em,
        c("Quorum parameter must be a logical vector or a list of quorum functions (see ?quorum_functions)",
          "length(quorum) == nrow(votes_matrix) is not TRUE"))
})

test_that("fuzzy params helpers", {
    votes_matrix = matrix(c(123, 912, 312, 45, 714, 255, 815, 414, 215), nrow = 3)
    district_seats = c(7, 5, 8)

    # weight_votes_matrix
    em = fuzzy_errors(function(votes_matrix) weight_votes_matrix(votes_matrix, district_seats))
    expect_identical(
        em,
        c("`votes_matrix` must be a matrix", "Votes in `votes_matrix` must be numbers >= 0",
          "`votes_matrix` must have districts as columns and parties as rows"
        ))
    em = fuzzy_errors(function(district_seats) weight_votes_matrix(votes_matrix, district_seats))
    expect_identical(
        em,
        c("`district_seats` must be a numeric vector, data.frame or a single number",
          "`length(district_seats)` must be the same as `ncol(votes_matrix)`",
          "`district_seats` must be integers", "`votes_matrix` must have districts as columns and parties as rows",
          "is.atomic(district_seats) is not TRUE"))

    # district_winner_matrix
    district_seats = 1L
    em = fuzzy_errors(function(votes_matrix) district_winner_matrix(votes_matrix, district_seats))
    expect_identical(
        em,
        c("is.matrix(votes_matrix) && ncol(votes_matrix) > 0 is not TRUE", "Votes in `votes_matrix` must be numbers >= 0"))

    em = fuzzy_errors(function(district_seats) district_winner_matrix(votes_matrix, district_seats))
    expect_true(all(
        em == "is.atomic(district_seats) && is.numeric(district_seats) && length(district_seats) >= 1 is not TRUE" |
            em == "`votes_matrix` must have districts as columns and parties as rows" |
            endsWith(em, "must be integers") |
            endsWith(em, "must be a numeric vector, data.frame or a single number")))

    # ceil_at
    x = c(0.5, 1.5, 2.5, 3)
    threshold = 0.5
    em = fuzzy_errors(function(x) ceil_at(x, threshold))
    expect_identical(
        em,
        "all(!is.na(x)) && all(is.numeric(x)) && all(x >= 0) is not TRUE")
    em = fuzzy_errors(function(threshold) ceil_at(x, threshold))
    expect_identical(
        em,
        c("length(threshold) == 1 && !is.na(threshold) is not TRUE",
          "Numeric value, \"harmonic\" or \"geometric\" expected for threshold argument.",
          "Threshold argument must be in [0,1]"))

    expect_true(is.null(fuzzy_errors(function(biproporz_result) get_divisors(biproporz_result))))
})

test_that("fuzzy params pivot", {
    df_long = data.frame(party = c("A", "A", "A", "B", "B", "B"),
                         region = c("III", "II", "I", "I", "II", "III"),
                         seats = c(5L, 3L, 1L, 2L, 4L, 6L))
    matrix_wide = matrix(1:6, nrow = 2)
    value_colname = "x"

    em = fuzzy_errors(function(df_long) pivot_to_matrix(df_long))
    expect_identical(em, "ncol(df_long) == 3 is not TRUE")

    em = fuzzy_errors(function(matrix_wide) pivot_to_df(matrix_wide, value_colname))
    expect_identical(em, "is.matrix(matrix_wide) is not TRUE")
})

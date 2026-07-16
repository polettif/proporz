test_that("shiny_create_empty_votes_matrix", {
    m = shiny_create_empty_votes_matrix(nrows = 3, ncols = 2)
    expect_identical(dim(m), c(3L, 2L))
    expect_identical(colnames(m), paste0("District ", 1:2))
    expect_identical(rownames(m), paste0("Party ", 1:3))

    m_single = shiny_create_empty_votes_matrix(nrows = 2, ncols = 1)
    expect_identical(dim(m_single), c(2L, 1L))
    expect_true(all(colnames(m_single) == "District 1"))

    m_onerow = shiny_create_empty_votes_matrix(nrows = 1, ncols = 4)
    expect_identical(dim(m_onerow), c(1L, 4L))
})

test_that("shiny_create_seats_matrix", {
    votes = shiny_create_empty_votes_matrix(nrows = 2, ncols = 3)
    district_seats = setNames(c(2, 1, 3), colnames(votes))
    s = shiny_create_seats_matrix(votes, district_seats)

    expect_identical(dim(s), c(1L, 3L))
    expect_identical(colnames(s), paste0("District ", 1:3))
    expect_identical(rownames(s), "seats")

    votes_single = shiny_create_empty_votes_matrix(nrows = 2, ncols = 1)
    district_seats_single <- 5 # Should default to total seats
    s_single = shiny_create_seats_matrix(votes_single, district_seats = district_seats_single)
    expect_identical(dimnames(s_single), list("seats", "total"))
})

test_that("shiny_get_quorum_function", {
    q1 = shiny_get_quorum_function(q_districts = 0.1, q_total = 0.2, q_all = TRUE)
    expect_true(is_quorum_function_list(q1))
    expect_length(q1, 2)

    q2 = shiny_get_quorum_function(q_districts = 0.1, q_total = 0.2, q_all = FALSE)
    expect_true(is_quorum_function_list(q2))
    expect_length(q2, 2)

    q3 = shiny_get_quorum_function(q_districts = 0.1, q_total = 0, q_all = TRUE)
    expect_true(is_quorum_function_list(q3))
    expect_length(q3, 1)

    q4 = shiny_get_quorum_function(q_districts = 0, q_total = 0.2)
    expect_true(is_quorum_function_list(q4))
    expect_length(q4, 1)

    q5 = shiny_get_quorum_function(q_districts = 0, q_total = 0, q_all = TRUE)
    expect_equal(q5, NULL)

    expect_true(is_quorum_function_list(shiny_get_quorum_function(q_districts = NULL, q_total = 0.1)))
    expect_true(is_quorum_function_list(shiny_get_quorum_function(q_districts = 0.1, q_total = NULL)))
})

test_that("shiny_check_uploaded_csv", {
    df = data.frame(Party = c("A"), District1 = c(10), District2 = c(5))
    expect_identical(shiny_check_uploaded_csv(df), "Input CSV must have at least 2 rows (excluding header with district names)")

    df = data.frame()
    expect_identical(shiny_check_uploaded_csv(df), "Input CSV must have at least 2 rows (excluding header with district names)")

    df = data.frame(Party = c("A", "B"))
    expect_identical(shiny_check_uploaded_csv(df), "Input CSV must have at least 2 columns (one of them party names)")

    df = data.frame(Party = c("A", "", ""), District1 = c(10, 5, 2))
    expect_identical(shiny_check_uploaded_csv(df), "Input CSV must have party names in first column")

    df = data.frame(Party = c("A", "B", ""), District1 = c(10, 5, 2), District2 = c(7, 8, 3))
    expect_identical(shiny_check_uploaded_csv(df), df)

    df[1, 2] <- NA
    expect_identical(shiny_check_uploaded_csv(df), "Input CSV must not have missing values")

    df <- data.frame(Party = c("A", NA), District1 = c(10, 5))
    expect_identical(shiny_check_uploaded_csv(df), "Input CSV must have party names in first column")
})

test_that("shiny_read_input_csv", {
    expect_identical(shiny_read_input_csv(test_path("data/shiny-input-error.csv")),
                     "Input CSV must not have missing values")
    x1 = shiny_read_input_csv(test_path("data/shiny-input-votes.csv"))
    x2 = shiny_read_input_csv(test_path("data/shiny-input-votes-seats.csv"))
    expect_identical(x1$votes, uri2020$votes_matrix)
    expect_identical(x2$votes, uri2020$votes_matrix)
    expect_equal(x2$seats, uri2020$seats_vector, tolerance = 1e-12)

    tmp.csv = tempfile(fileext = ".csv")
    shiny_write_input_csv(x2$votes, x2$seats, tmp.csv)
    x2_reread = shiny_read_input_csv(tmp.csv)
    expect_equal(x2, x2_reread)

    tmp2.csv = tempfile(fileext = ".csv")
    shiny_write_input_csv(shinyapp_examples$zug_2018$votes, shinyapp_examples$zug_2018$seats, tmp2.csv)
    zug_reread = shiny_read_input_csv(tmp2.csv)
    exp = shinyapp_examples$zug_2018
    names(dimnames(exp$votes)) <- NULL
    expect_equal(zug_reread, exp)
})

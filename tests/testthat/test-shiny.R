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
})

fuzzy_params = fuzzr::test_all()

fuzzy_params$mtx_complete_named <- uri2020$votes_matrix
fuzzy_params$mtx_complete_unnamed <- unname(uri2020$votes_matrix)
fuzzy_params$mtx_with_na <- local({
    mtx = unname(uri2020$votes_matrix)
    mtx[c(3,5,7,14)] <- NA
    mtx
})
fuzzy_params$list_empty <- list()
fuzzy_params$list_dbl <- 1.5

saveRDS(fuzzy_params, "../tests/testthat/fuzzy_params.rds")

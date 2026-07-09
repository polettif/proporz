testthat::test_dir(".")
covr::report(covr::file_coverage("bazi.R", "test-read_bazi_data.R"))

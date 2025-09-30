test_that("Uri 2020", {
    biproporz_uri = function(votes_matrix, seats, ...) {
        biproporz(votes_matrix, seats)
    }

    uri20_result = structure(
        c(5L, 4L, 3L, 3L, 2L, 1L, 1L,3L,
          2L, 2L, 1L, 1L, 3L, 2L, 2L, 2L),
        .Dim = c(4L, 4L), .Dimnames = list(
            c("CVP", "SPGB", "FDP", "SVP"),
            c("Altdorf", "B\u00fcrglen", "Erstfeld", "Schattdorf")))

    uri20_output = biproporz(uri2020$votes_matrix, uri2020$seats_vector)
    expect_identical(unname(as.matrix(uri20_output)), unname(uri20_result))
    expect_identical(rownames(uri20_output), rownames(uri20_result))
    expect_identical(colnames(uri20_output), colnames(uri20_result))
    expect_false(is.null(get_divisors(uri20_output)))
})

test_that("Zug 2018", {
    pukelsheim_zug = function(votes_df, district_seats_df) {
        pukelsheim(votes_df, district_seats_df,
                   quorum = quorum_any(any_district = 0.05, total = 0.03),
                   winner_take_one = TRUE)
    }

    # https://www.zg.ch/behoerden/staatskanzlei/kanzlei/abstimmungen-und-wahlen/wahlen-kr/archiv-2018/downloads/2018-kr-dpt-gesamtergebnis-sitzverteilung.pdf
    seats_matrix_expected = structure(as.integer(
        c(0, 3, 3, 5, 2, 3, 3, 0, 0, 2, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 2, 0, 0, 1, 1, 0, 0, 1,
          0, 2, 3, 2, 1, 3, 4, 0, 1, 3, 2, 1, 1, 2, 0, 1, 2, 1, 0, 1, 1, 0, 2, 2, 1, 0, 0, 1,
          0, 1, 2, 2, 0, 0, 2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1)), dim = c(7L, 11L),
        dimnames = list(
            list_id = c("1", "2", "3", "4", "5", "6", "7"),
            entity_name = c("Zug", "Ober\u00e4geri", "Unter\u00e4geri", "Menzingen", "Baar", "Cham",
                "H\u00fcnenberg", "Steinhausen", "Risch", "Walchwil", "Neuheim")))

    # check pukelsheim result is equal to actual result
    votes_df = unique(zug2018[c("list_id", "entity_name", "list_votes")])
    district_seats_df = unique(zug2018[c("entity_name", "election_mandates")])

    seats_df = pukelsheim_zug(votes_df,
                              district_seats_df)

    seats_mtrx = pivot_to_matrix(seats_df[c(1,2,4)])
    expect_identical(seats_mtrx[,colnames(seats_matrix_expected)], seats_matrix_expected)

    # previous behavior check: same result without winner_take_one
    seats_df_wtoFALSE = pukelsheim(votes_df, district_seats_df,
                                   quorum = quorum_any(any_district = 0.05, total = 0.03),
                                   winner_take_one = FALSE)
    expect_true(all(seats_df_wtoFALSE == seats_df))

    # check with matrix
    seats_mtrx_wto = biproporz(pivot_to_matrix(votes_df),
                               district_seats_df,
                               method = "wto",
                               quorum = quorum_any(any_district = 0.05,
                                                   total = 0.03))
    expect_identical(max(seats_mtrx_wto-seats_mtrx), 0L)
})

test_that("Grisons 2022", {
    grisons2022 = testdata$GR_2022
    biproporz_grisons = function(votes_matrix, seats, ...) {
        biproporz(votes_matrix, seats,
                  quorum = quorum_all(total = 0.03),
                  method = "wto")
    }

    seats_wto = expect_warning(
        biproporz_grisons(t(grisons2022$votes_matrix), grisons2022$district_seats_df),
        "Not enough seats for tied parties with the most votes in: 'Rheinwald'\nWinner take one condition is not applied in this district.")
    expect_equal(as.matrix(t(seats_wto)), grisons2022$expected_result, tolerance = 1e-14)
})

test_that("Zurich 2019", {
    zurich2019 = testdata$ZH_2019
    pukelsheim_zurich = function(votes_df, district_seats_df) {
        pukelsheim(votes_df, district_seats_df,
                   quorum = quorum_any(any_district = 0.05), new_seats_col = "Sitze")
    }

    seats_expected = zurich2019$expected_result[order(zurich2019$expected_result$Liste,
                                                      zurich2019$expected_result$Wahlkreis),]

    seats_actual = pukelsheim_zurich(zurich2019$votes_df, zurich2019$district_seats_df)
    seats_actual <- seats_actual[order(seats_actual$Liste, seats_actual$Wahlkreis),
                                 c("Liste", "Wahlkreis", "Sitze")]

    expect_identical(seats_actual, seats_expected)
})

test_that("Aargau 2020", {
    aargau2020 = testdata$AG_2020
    pukelsheim_aargau = function(votes_df, district_seats_df) {
        pukelsheim(votes_df, district_seats_df,
                   quorum = quorum_any(any_district = 0.05, total = 0.03),
                   weight_votes = FALSE) # the test data set uses "Wählerzahl"
    }

    seats_expected = aargau2020$expected_result
    seats_actual = pukelsheim_aargau(aargau2020$votes_df, aargau2020$district_seats_df)

    seats_compare = merge(seats_expected, seats_actual, by = c("Partei", "Bezirk"))
    expect_identical(seats_compare$Sitze, seats_compare$seats)
})

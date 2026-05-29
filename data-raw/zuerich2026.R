# dataset is not part of the package
# this is just a basic check to ensure proporz matches the official results

# https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/734@statistisches-amt-kanton-zuerich

library(proporz)
library(dplyr)

json = jsonlite::read_json("https://ogd-static.voteinfo-app.ch/v4/ogd/proporz_resultate_2026_03_08.json")

stadt_zuerich = json$kantone[[2]]$vorlagen[[8]]$wahlkreise |>
    lapply(\(x) {
    dplyr::bind_rows(x$resultat$listen) |>
        mutate(
            wahlkreisNummer = x$wahlkreisNummer,
            wahlkreisBezeichnung = x$wahlkreisBezeichnung,
            anzahlSitze = x$anzahlSitze)
}) |>
    bind_rows() |>
    mutate(wahlkreisBezeichnung = gsub("Zürich ", "", wahlkreisBezeichnung))

votes_df = stadt_zuerich |>
    select(listeCode, wahlkreisBezeichnung, stimmen)

seats_df = stadt_zuerich |>
    distinct(wahlkreisBezeichnung, anzahlSitze)

pk = pukelsheim(votes_df, seats_df, quorum_any(0.05, 0.3))

get_divisors(pk)

expected = stadt_zuerich |>
    select(listeCode, wahlkreisBezeichnung, seats_expected = sitze)

check = left_join(pk, expected, c("listeCode", "wahlkreisBezeichnung"))

testthat::expect_identical(check$seats, check$seats_expected)

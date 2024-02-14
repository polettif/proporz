uri2020 = list(
    votes_matrix = structure(
        c(11471L, 11908L, 9213L, 7756L, 2822L,
          1606L, 1567L, 2945L, 2309L, 1705L,
          946L, 1573L, 4794L, 2600L, 2961L, 3498L),
        .Dim = c(4L, 4L),
        .Dimnames = list(
            c("CVP", "SPGB", "FDP", "SVP"),
            c("Altdorf", "B\u00fcrglen", "Erstfeld", "Schattdorf"))),
    seats_vector = c(15, 7, 6, 9)
)

names(uri2020$seats_vector) <- colnames(uri2020$votes_matrix)

usethis::use_data(uri2020, overwrite = TRUE)

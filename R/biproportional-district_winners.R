row_maxs = function(mtrx) {
    apply(mtrx, 1, max)
}

col_maxs = function(mtrx) {
    apply(mtrx, 2, max)
}

district_winner_matrix = function(votes_matrix, ties = "error") {
    .district_max_matrix = matrix(rep(unname(col_maxs(votes_matrix)),
                                      each = nrow(votes_matrix)), nrow(votes_matrix))
    district_winners = votes_matrix == .district_max_matrix

    # find tied districts
    tied_districts = which(colSums(district_winners) > 1)

    if(length(tied_districts) > 0) {
        if(ties == "error") {
            if(!is.null(names(tied_districts))) {
                .ties_stop = paste(names(tied_districts), collapse = "', '")
                .ties_stop <- paste0("'", .ties_stop, "'")
            } else {
                .ties_stop <- paste0("column ", paste(tied_districts, collapse = ", "))
            }
            stop("Tied majority in ", .ties_stop)
        } else if(ties == "random") {
            # apply tiebreak
            # TODO extract in function
            for(district in tied_districts) {
                tied_parties = which(district_winners[,district])

                tiebreak_winner = unname(sample(tied_parties, 1))
                tiebreak_loser = setdiff(seq_len(nrow(votes_matrix)), tiebreak_winner)
                assert(length(tiebreak_winner) == 1)

                district_winners[tiebreak_loser,district] <- FALSE
            }
        } else {
            stop("ties param must be 'error' or 'random'", call. = FALSE)
        }
    }

    return(district_winners)
}

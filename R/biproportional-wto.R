row_maxs = function(mtrx) {
    apply(mtrx, 1, max)
}

col_maxs = function(mtrx) {
    apply(mtrx, 2, max)
}

has_tied_district_winners = function(votes_matrix) {
    district_winner_matrix(votes_matrix, TRUE)
}

district_winner_matrix = function(votes_matrix, return_check = FALSE) {
    if(is.null(colnames(votes_matrix))) {
        stop("votes matrix must have district column names", call. = FALSE)
    }
    .districts_max = unname(col_maxs(votes_matrix))
    .district_max_matrix = matrix(rep(.districts_max, each = nrow(votes_matrix)),
                                  nrow(votes_matrix))
    district_winners = votes_matrix == .district_max_matrix

    # find tied districts
    tied_districts = which(colSums(district_winners) > 1)
    if(return_check) {
        return(length(tied_districts) > 0)
    }

    # error on tied districts
    if(length(tied_districts) > 0) {
        .ties_stop = paste(names(tied_districts), collapse = "', '")
        .ties_stop <- paste0("'", .ties_stop, "'")
        stop("Tied majority in ", .ties_stop, call. = FALSE)
    }

    return(district_winners)
}

create_wto_round_function = function(votes_matrix, seats_ua) {
    if(is.null(votes_matrix) || is.null(rownames(votes_matrix))) {
        stop("votes_matrix must have column and row names to handle district winners",
             call. = FALSE)
    }

    district_winners = district_winner_matrix(votes_matrix)

    # "Dies wird erreicht, indem in jedem Wahlkreis bei der stimmenstärksten Liste -
    # und nur jeweils dort - der Quotient aus Parteistimmen und Wahlkreis- und
    # Parteidivisoren auch dann aufgerundet [wird], wenn er unter 0.5 liegt."
    #   Pukelsheim F.; Schumacher C. (2011):
    #   Doppelproporz bei Parlamentswahlen. Ein Rück- und Ausblick.
    district_winner_round_func = function(x) {
        district_winners_subset = district_winners[rownames(x),colnames(x),drop=F]

        x_winners <- x_others <- x
        x_winners[which(!district_winners_subset)] <- 0
        x_others[which(district_winners_subset)] <- 0

        # round all district winners to 1, standard rounding after that
        y_winners = ceil_at(x_winners, "at_least_one")
        # standard rounding for everyone else
        y_others = ceil_at(x_others, 0.5)

        y = y_winners+y_others
        dimnames(y) <- dimnames(x)
        return(y)
    }

    # check if there are enough seats for each party to satisfy winner constraint
    seats_dw = rowSums(district_winners)
    if(!all(seats_dw<=seats_ua)) {
        parties = names(seats_ua)[seats_dw>seats_ua]
        parties <- paste(parties, collapse = ", ")
        stop("Not enough upper apportionment seats to give district winner seats to party/list ",
             parties, call. = FALSE)
    }

    return(district_winner_round_func)
}

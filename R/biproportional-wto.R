row_maxs = function(mtrx) {
    apply(mtrx, 1, max)
}

col_maxs = function(mtrx) {
    apply(mtrx, 2, max)
}

most_votes_in_district_matrix = function(votes_matrix) {
    .districts_max = unname(col_maxs(votes_matrix))
    .district_max_matrix = matrix(rep(.districts_max, each = nrow(votes_matrix)),
                                  nrow(votes_matrix))
    return(votes_matrix == .district_max_matrix)
}

create_wto_round_function = function(votes_matrix, seats_districts, seats_parties) {
    if(is.null(colnames(votes_matrix)) || is.null(rownames(votes_matrix))) {
        stop("votes_matrix must have column and row names to handle district winners",
             call. = FALSE)
    }

    district_winners = most_votes_in_district_matrix(votes_matrix)

    # Check if there are more winners than seats in any district
    not_enough_district_seats = which(colSums(district_winners) > seats_districts)
    if(length(not_enough_district_seats) > 0) {
        district_winners[,not_enough_district_seats] <- FALSE

        warning("Not enough seats for tied parties with the most votes in: ",
                collapse_names(names(not_enough_district_seats)),
                "\nWinner take one condition is not applied in ",
                ifelse(length(not_enough_district_seats) == 1, "this district.", "these districts."),
                call. = FALSE)
    }

    # check if there are enough seats for each party to satisfy winner constraint
    not_enough_party_seats = which(rowSums(district_winners) > seats_parties)
    if(length(not_enough_party_seats) > 0) {
        stop("Not enough upper apportionment seats to give district winner seats to party/list: ",
             collapse_names(names(not_enough_party_seats)), call. = FALSE)
    }

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

    return(district_winner_round_func)
}

most_votes_in_district_matrix = function(votes_matrix) {
    district_max_matrix = row_as_matrix(col_maxs(votes_matrix), votes_matrix)
    return(votes_matrix == district_max_matrix)
}

create_wto_round_function = function(votes_matrix, district_seats, seats_parties) {
    if(is.null(colnames(votes_matrix)) || is.null(rownames(votes_matrix))) {
        stop("votes_matrix must have column and row names to handle district winners",
             call. = FALSE)
    }

    district_winners = most_votes_in_district_matrix(votes_matrix)

    # Check if there are more winners than seats in any district
    not_enough_district_seats = which(colSums(district_winners) > district_seats)
    if(length(not_enough_district_seats) > 0) {
        district_winners[,not_enough_district_seats] <- FALSE

        warning("Not enough seats for tied parties with the most votes in: ",
                collapse_names(names(not_enough_district_seats)),
                "\nWinner take one condition is not applied in ",
                num_word("this district.", "these districts.", not_enough_district_seats),
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
        district_winners_subset = district_winners[rownames(x),colnames(x),drop=FALSE]

        x_winners <- x_others <- x
        x_winners[which(!district_winners_subset)] <- 0
        x_others[which(district_winners_subset)] <- 0

        # round all district winners to 1, standard rounding after that
        y_winners = ceil_at(x_winners, "0.5_at_least_one")
        # standard rounding for everyone else
        y_others = ceil_at(x_others, 0.5)

        y = y_winners+y_others
        dimnames(y) <- dimnames(x)
        return(y)
    }

    return(district_winner_round_func)
}

#' Find which party has the most votes in a district
#'
#' Create a logical matrix that shows whether a party got the most votes in a district or not.
#'
#' @inheritParams upper_apportionment
#' @param district_seats Vector defining the number of seats per district. Must be the same
#'   length as `ncol(votes_matrix)`. Values are name-matched to `votes_matrix` columns if both
#'   are named. If a single value is supplied (like `1` as default), it is used as the number of
#'   seats for every district.
#'
#' @return logical matrix with the same dimensions and names as `votes_matrix`
#'
#' @details If two or more parties are tied and there are not enough seats for each tied party,
#'   the matrix value is `NA`.
#'
#' @export
#'
#' @examples
#' (vm = matrix(c(60,30,0,20,10,30), nrow = 3, dimnames = list(1:3, c("A", "B"))))
#'
#' district_winner_matrix(vm)
#'
#' # NA values if parties are tied (here in district B)
#' vm[1,2] <- 30
#' district_winner_matrix(vm)
#'
#' # No NA values for tied parties if enough seats are available
#' district_winner_matrix(vm, c(1, 2))
district_winner_matrix = function(votes_matrix,
                                  district_seats = 1L) {
    if(length(district_seats) == 1L) {
        district_seats <- rep(district_seats, ncol(votes_matrix))
    }
    if(is.null(names(district_seats))) names(district_seats) <- colnames(votes_matrix)
    .votes_matrix.name = deparse(substitute(votes_matrix))
    .district_seats.name = deparse(substitute(district_seats))
    votes_matrix <- prep_votes_matrix(votes_matrix, .votes_matrix.name)
    district_seats <- prep_district_seats(district_seats, votes_matrix, .district_seats.name, .votes_matrix.name)

    most_votes = most_votes_in_district_matrix(votes_matrix)
    not_enough_district_seats = which(colSums(most_votes) > district_seats)
    if(length(not_enough_district_seats) > 0) {
        ties_01 = (col(votes_matrix) %in% not_enough_district_seats) * most_votes
        most_votes[ties_01 == 1] <- NA
    }

    return(most_votes)
}

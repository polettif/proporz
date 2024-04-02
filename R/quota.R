#' Largest remainder method
#'
#' Allocate seats based on the largest fractional remainder. The largest remainder method is
#' also known as: Hamilton, Hare-Niemeyer or Vinton method.
#'
#' The numbers of votes for each party is divided by a quota representing the number of
#' votes required for a seat. Then, each party receives the rounded down quota value as
#' seats. The remaining seats are given to the party with the largest remainder until all
#' seats have been distributed.
#'
#' @inheritParams proporz
#' @seealso [proporz()]
#' @inherit proporz return
#'
#' @note Only the quota `total votes / total seats` (which is used by the aforementioned
#'   methods) is implemented.
#'
#' @examples
#' votes = c(47000, 16000, 15800, 12000, 6100, 3100)
#' largest_remainder_method(votes, 10)
#'
#' @export
largest_remainder_method = function(votes, n_seats, quorum = 0) {
    check_votes_vector(votes, deparse(substitute(votes)))
    check_seats_number(n_seats, deparse(substitute(n_seats)))

    if(length(votes) == 1) {
        return(n_seats)
    }
    if(n_seats == 0) {
        return(rep(0, length(votes)))
    }

    # apply quorum
    votes <- apply_quorum_vector(votes, quorum)

    # get LR-quota and assign seats
    quota = lr_quota(votes, n_seats)
    quotas = votes / quota

    seats_base = floor(quotas)
    seats_remainder = rep(0, length(votes))

    if(sum(seats_base) < n_seats) {
        remainders = quotas - seats_base
        n_seats_remaining = n_seats - sum(seats_base)
        ordered_remainders = order(remainders, decreasing = TRUE)
        check_equal_entries(remainders, ordered_remainders, n_seats_remaining)

        seats_remainder[ordered_remainders[1:n_seats_remaining]] <- 1
    }

    seats = as.integer(seats_base + seats_remainder)

    if(!is.null(names(votes))) {
        names(seats) <- names(votes)
    }

    return(seats)
}

lr_quota = function(votes, n_seats, method = "hare") {
    if(method %in% c("hare", "hare-niemeyer", "vinton", "simple")) {
        quota = sum(votes)/n_seats
    } else {
        stop("Unknown quota method '", method, "'", call. = FALSE)
    }
    return(quota)
}

check_equal_entries = function(remainders, ordered_remainders, n_seats_remaining) {
    remainder_last_with = remainders[ordered_remainders[n_seats_remaining]]
    remainder_first_without = remainders[ordered_remainders[n_seats_remaining+1]]

    if(remainder_last_with == remainder_first_without) {
        indices = unique(which(remainders == remainder_last_with, arr.ind = TRUE))
        parties = collapse_names(indices)
        if(!is.null(names(remainders))) {
            parties <- collapse_names(names(remainders)[indices])
        }
        stop("Result is undefined, equal remainder for parties: ", parties,
             call. = FALSE)
    }
}

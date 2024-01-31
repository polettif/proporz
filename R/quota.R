#' Largest remainder method
#'
#' Also known as:  Hamilton, Hare-Niemeyer, Vinton method
#'
#' @inheritParams proporz
#' @seealso \code{\link{proporz}}
#' @inherit proporz return
#' @export
quota_largest_remainder = function(votes, n_seats, quorum = 0) {
    check_votes_vector(votes, deparse(substitute(votes)))
    check_seats_number(n_seats, deparse(substitute(n_seats)))

    if(length(votes) == 1) {
        return(n_seats)
    }
    if(n_seats == 0) {
        return(rep(0, length(votes)))
    }

    votes <- apply_quorum_vector(votes, quorum)

    # calculate
    quota = n_seats*votes/sum(votes)
    seats_base = floor(quota)
    seats_remainder = rep(0, length(votes))

    if(sum(seats_base) < n_seats) {
        remainders = quota - seats_base
        n_seats_remaining = n_seats - sum(seats_base)
        ordered_remainders = order(remainders, decreasing = TRUE)
        check_equal_entries(remainders, ordered_remainders, n_seats_remaining)

        seats_remainder[ordered_remainders[1:n_seats_remaining]] <- 1
    }

    return(seats_base + seats_remainder)
}

check_equal_entries = function(remainders, ordered_remainders, n_seats_remaining) {
    remainder_last_with = remainders[ordered_remainders[n_seats_remaining]]
    remainder_first_without = remainders[ordered_remainders[n_seats_remaining+1]]

    if(remainder_last_with == remainder_first_without) {
        indices = which(remainders == remainder_last_with, arr.ind = TRUE)
        parties = paste0(indices, collapse = " & ")
        stop("Result is undefined, equal remainder for parties: ", parties,
             call. = F)
    }
}

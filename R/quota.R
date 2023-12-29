#' Largest remainder method
#'
#' Also known as:  Hamilton, Hare-Niemeyer, Vinton method
#'
#' @inheritParams proporz
#' @param throw_equal_remainder_error Should an error be thrown if two parties
#'                                    have the same remainder? Default is TRUE
#'                                    since apportionment behavior is undefined.
#' @seealso \code{\link{proporz}}
#' @export
quota_largest_remainder = function(votes, n_seats, throw_equal_remainder_error = TRUE) {
    check_n_seats(n_seats)
    check_votes(votes)

    if(length(votes) == 1) {
        return(n_seats)
    }
    if(n_seats == 0) {
        return(rep(0, length(votes)))
    }

    quota = n_seats*votes/sum(votes)
    seats_base = floor(quota)
    seats_rem = rep(0, length(votes))

    if(sum(seats_base) < n_seats) {
        remainder = quota - seats_base
        check_equal_entries(remainder[remainder > 0])

        n_seats_remaining = n_seats - sum(seats_base)
        seats_rem <- rep(0, length(votes))
        order_index = order(remainder, decreasing = TRUE)
        seats_rem[order_index[1:n_seats_remaining]] <- 1
    }

    return(seats_base + seats_rem)
}

check_equal_entries = function(vec) {
    stopifnot(is.vector(vec))
    if(length(unique(vec)) != length(vec)) {
        eq_index = which(vec == names(which.max(table(vec))))
        eq_str = paste0(eq_index, collapse = " & ")
        stop("Result is undefined: Equal remainder for two parties (position ", eq_str ,")",
             call. = FALSE)
    }
}


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
    quota = n_seats*votes/sum(votes)
    seats_base = floor(quota)

    remainder = quota - seats_base
    check_equal_entries(remainder, "remainder")

    n_seats_remaining = n_seats - sum(seats_base)
    seats_rem <- rep(0, length(votes))
    order_index = order(remainder, decreasing = TRUE)
    seats_rem[order_index[1:n_seats_remaining]] <- 1

    return(seats_base + seats_rem)
}

#' Highest averages method
#'
#' Allocate seats proportionally for \code{\link[=divisor_methods]{divisor methods}}.
#'
#' The highest averages method requires the number of votes for each party to
#' be divided successively by a series of divisors. This produces a table of
#' quotients, or averages, with a row for each divisor and a column for each
#' party. The nth seat is allocated to the party whose column contains the nth
#' largest entry in this table, up to the total number of seats
#' available. (\href{https://en.wikipedia.org/wiki/Highest_averages_method}{Wikipedia})
#'
#' @inheritParams proporz
#' @param divisors sequence of divisors (length equal to the number of seats).
#'                 If it is a single number (e.g. 0.5), a sequence is generated
#'                 starting with it.
#'
#' @inherit proporz return
#'
#' @examples
#' highest_averages_method(c(5200, 1700, 3100), 15, 0.5)
#'
#' highest_averages_method(votes = c(50, 0, 30), n_seats = 3,
#'                         divisors = c(0, 1.3333, 2.4))
#' @export
highest_averages_method = function(votes, n_seats, divisors) {
    check_votes_vector(votes, deparse(substitute(votes)))
    check_seats_number(n_seats, deparse(substitute(n_seats)))

    if(length(votes) == 1) { return(n_seats) }
    stopifnot(all(!is.na(votes)))
    if(n_seats == 0) { return(rep(0, length(votes))) }

    stopifnot(is.null(dim(divisors)))
    if(length(divisors) == 1) {
        divisors <- seq(from = divisors, by = 1, length.out = n_seats)
    } else if(length(divisors) != n_seats) {
        stop("Number of divisors is not equal to the number of seats", call. = FALSE)
    }
    n_parties = length(votes)

    # method
    mtrx_votes = matrix(rep(votes, each=n_seats), ncol = n_parties)
    mtrx_divisors = matrix(rep(divisors, ncol(mtrx_votes)), ncol = n_parties)

    mtrx_quotient = mtrx_votes/mtrx_divisors
    check_edge_quotient(mtrx_quotient, n_seats)

    # assign seats
    mtrx_seats = mtrx_quotient-mtrx_quotient # 0 filled matrix
    mtrx_seats[order(mtrx_quotient, decreasing = TRUE)[1:n_seats]] <- 1

    vec = colSums(mtrx_seats)
    vec[is.nan(vec)] <- 0
    names(vec) <- names(votes)

    return(vec)
}

#' Divisor methods
#'
#' Functions to directly apply divisor apportionment methods instead
#' of calling [proporz()] with a method parameter.
#'
#' Divisor methods are known under different names:
#' `r .doc_proporz_methods(TRUE)`
#'
#' All divisor functions call [highest_averages_method()] with a different sequence of
#' divisors.
#'
#' @inheritParams proporz
#' @seealso [proporz()]
#' @inherit proporz return
#' @examples
#' votes = c("Party A" = 690, "Party B" = 400,
#'           "Party C" = 250, "Party D" = 120)
#'
#' divisor_round(votes, 10)
#'
#' divisor_floor(votes, 10)
#'
#' divisor_ceiling(votes, 10)
#'
#' divisor_ceiling(votes, 5)
#'
#' divisor_geometric(votes, 10, quorum = 0.05)
#'
#' divisor_harmonic(votes, 10)
#' @name divisor_methods
NULL

#' @rdname divisor_methods
#' @export
divisor_round = function(votes, n_seats, quorum = 0) {
    check_votes_vector(votes, deparse(substitute(votes)))
    check_seats_number(n_seats, deparse(substitute(n_seats)))
    votes <- apply_quorum_vector(votes, quorum)
    highest_averages_method(votes, n_seats, 0.5)
}

#' @rdname divisor_methods
#' @export
divisor_floor = function(votes, n_seats, quorum = 0) {
    check_votes_vector(votes, deparse(substitute(votes)))
    check_seats_number(n_seats, deparse(substitute(n_seats)))
    votes <- apply_quorum_vector(votes, quorum)
    highest_averages_method(votes, n_seats, 1)
}

#' @rdname divisor_methods
#' @export
divisor_harmonic = function(votes, n_seats, quorum = 0) {
    check_votes_vector(votes, deparse(substitute(votes)))
    check_seats_number(n_seats, deparse(substitute(n_seats)))

    divisors = seq_harmonic(seq(1, n_seats))

    votes <- apply_quorum_vector(votes, quorum)
    check_enough_seats(votes, n_seats, "harmonic")
    highest_averages_method(votes, n_seats, divisors)
}

#' @rdname divisor_methods
#' @export
divisor_geometric = function(votes, n_seats, quorum = 0) {
    check_votes_vector(votes, deparse(substitute(votes)))
    check_seats_number(n_seats, deparse(substitute(n_seats)))

    divisors = seq_geometric(seq(1, n_seats))

    votes <- apply_quorum_vector(votes, quorum)
    check_enough_seats(votes, n_seats, "geometric")
    highest_averages_method(votes, n_seats, divisors)
}

#' @rdname divisor_methods
#' @export
divisor_ceiling = function(votes, n_seats, quorum = 0) {
    check_votes_vector(votes, deparse(substitute(votes)))
    check_seats_number(n_seats, deparse(substitute(n_seats)))

    votes <- apply_quorum_vector(votes, quorum)
    check_enough_seats(votes, n_seats, "ceiling")
    highest_averages_method(votes, n_seats, 0)
}

#' Highest averages method
#'
#' Allocates seats proportionally for divisor methods.
#'
#' The highest averages method requires the number of votes for each party to
#' be divided successively by a series of divisors. This produces a table of
#' quotients, or averages, with a row for each divisor and a column for each
#' party. The nth seat is allocated to the party whose column contains the nth
#' largest entry in this table, up to the total number of seats available.
#' (\href{https://en.wikipedia.org/wiki/Highest_averages_method}{Wikipedia})
#'
#' @param party_votes vector with votes for each party
#' @param n_seats total number of seats
#' @param divisors sequence of divisors. If it's a single number, a sequence
#'                 is generated starting with it.
#'
#' @export
highest_averages_method = function(party_votes, n_seats, divisors) {
	check_votes(party_votes)
	check_n_seats(n_seats)
	if(length(party_votes) == 1) { return(n_seats) }
	stopifnot(all(!is.na(party_votes)))
	if(n_seats == 0) { return(rep(0, length(party_votes))) }

    stopifnot(is.null(dim(divisors)))
    if(length(divisors) == 1) {
    	divisors <- seq(from = divisors, by = 1, length.out = n_seats)
    }
    n_parties = length(party_votes)

    # method
    mtrx_votes = matrix(rep(party_votes, each=n_seats), ncol = n_parties)
    mtrx_divisors = matrix(rep(divisors, ncol(mtrx_votes)), ncol = n_parties)

    mtrx_quotient = mtrx_votes/mtrx_divisors
    check_equal_entries(mtrx_quotient, "quotient")

    mtrx_seats <- mtrx_quotient-mtrx_quotient # 0 filled matrix
    mtrx_seats[order(mtrx_quotient, decreasing = TRUE)[1:n_seats]] <- 1

    vec = colSums(mtrx_seats)
    vec[is.nan(vec)] <- 0
    names(vec) <- names(party_votes)

    return(vec)
}

#' Highest averages method
#'
#' Alias for \code{\link{highest_averages_method}}. Named after its
#' German equivalent "Höchstzahlverfahren".
#'
#' @inheritParams highest_averages_method
#' @export
hzv = function(party_votes, n_seats, divisors) {
    highest_averages_method(party_votes, n_seats, divisors)
}

#' Divisor method rounding down
#'
#' Also known as D'Hondt, Jefferson, Hagenbach-Bischoff method
#'
#' @inheritParams proporz
#' @seealso \code{\link{proporz}}
#'
#' @export
divisor_floor = function(votes, n_seats, quorum = 0) {
    votes <- quorum_votes(votes, quorum)
    hzv(votes, n_seats, 1)
}

#' Divisor method standard rounding
#'
#' Also known as: Sainte-Laguë, Webster method
#'
#' @inheritParams proporz
#' @seealso \code{\link{proporz}}
#' @export
divisor_round = function(votes, n_seats, quorum = 0) {
    votes <- quorum_votes(votes, quorum)
    hzv(votes, n_seats, 0.5)
}

#' #' Divisor method rounding up
#'
#' Also known as: Adams method
#'
#' @inheritParams proporz
#' @seealso \code{\link{proporz}}
#' @export
divisor_ceiling = function(votes, n_seats, quorum = 0) {
    votes <- quorum_votes(votes, quorum)
    hzv(votes, n_seats, 10e-12)
}

#' Divisor method harmonic rounding
#'
#' Also known as: Dean method
#'
#' @inheritParams proporz
#' @seealso \code{\link{proporz}}
#' @export
divisor_harmonic = function(votes, n_seats, quorum = 0) {
	check_n_seats(n_seats)

    nn = seq(1, n_seats)
    divisors = 2/((1/nn)+(1/(nn-1)))
    divisors[0] <- 10e-12
    votes <- quorum_votes(votes, quorum)

    hzv(votes, n_seats, divisors)
}

#' Divisor method geometric rounding
#'
#' Also known as: Hill-Huntington method
#'
#' @inheritParams proporz
#' @seealso \code{\link{proporz}}
#' @export
divisor_geometric = function(votes, n_seats, quorum = 0) {
	check_n_seats(n_seats)

    nn = seq(1, n_seats)
    divisors = sqrt((nn-1)*nn)
    divisors[1] <- 10e-12
    votes <- quorum_votes(votes, quorum)

    hzv(votes, n_seats, divisors)
}

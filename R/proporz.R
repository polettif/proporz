#' Proportional Apportionment
#'
#' Calculate seat apportionment for legislative bodies.
#'
#' @param votes numeric vector with number of votes for each party
#' @param n_seats total number of seats
#' @param method Apportionment method to use, as character. Not case sensitive. See details.
#' @param quorum Vote threshold a party must reach. Used as quota of total
#'               votes within a district if less than 1 otherwise as number
#'               of votes.
#'
#' @note Seats can also be apportioned among regions instead of parties. The parameter
#'       \code{votes} is then normally used with census data (e.g. population counts).
#'
#' @details The following methods are available:
#' \itemize{
#'          \item{d'hondt, jefferson, hagenbach-bischoff, floor (use \code{\link{divisor_floor}})}
#'          \item{sainte-lague, webster, round (use \code{\link{divisor_round}})}
#'          \item{adams, ceiling (use \code{\link{divisor_ceiling}})}
#'          \item{dean, harmonic (use \code{\link{divisor_harmonic}})}
#'          \item{huntington-hill, hill-huntington, geometric (use \code{\link{divisor_geometric}})}
#'          \item{hare-niemeyer, hamilton, vinton, quota_largest_remainder (use \code{\link{quota_largest_remainder}})}
#' }
#' @returns The number of seats per party as a vector
#'
#' @examples
#' votes = c("Party A" = 651, "Party B" = 349, "Party C" = 50)
#'
#' proporz(votes, 10, "sainte-lague")
#'
#' proporz(votes, 10, "hill-huntington")
#'
#' proporz(votes, 10, "hill-huntington", quorum = 0.05)
#'
#' proporz(votes, 10, "jefferson", quorum = 70)
#'
#'@export
proporz = function(votes, n_seats, method, quorum = 0) {
    m = get_apport_method(method)
    apport_methods = list(
        "floor" = divisor_floor,
        "round" = divisor_round,
        "ceiling" = divisor_ceiling,
        "harmonic" = divisor_harmonic,
        "geometric" = divisor_geometric,
        "quota_largest_remainder" = quota_largest_remainder
    )
    apport_methods[[m]](votes, n_seats, quorum)
}

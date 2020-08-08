#' Proportional Apportionment
#'
#' Calculate seat apportionment for legislative bodies.
#'
#' @param votes numeric vector with number of votes for each party
#' @param n_seats total number of seats
#' @param method Apportionment method to use. Not case sensitive. See details.
#' @param quorum Vote threshold a party must reach. Used as quota of total
#'               votes within a district if less than 1 otherwise as number
#'               of votes.
#'
#' @details The following methods are available:
#' \itemize{
#'          \item{d'hondt, jefferson, hagenbach-bischoff, floor}
#'          \item{sainte-lague, webster, round}
#'          \item{adams, ceiling}
#'          \item{dean, harmonic}
#'          \item{huntington-hill, hill-huntington, geometric}
#'          \item{hare-niemeyer, hamilton, vinton, quota_largest_remainder}
#' }
#' @examples
#' votes = c("Party A" = 651, "Party B" = 349, "Party C" = 50)
#'
#' proporz(votes, 10, "sainte-lague")
#'
#' proporz(votes, 10, "hill-huntington")
#'
#' proporz(votes, 10, "hill-huntington", quorum = 0.05)
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

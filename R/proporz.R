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
#'          \item{d'hondt, jefferson, hagenbach-bischoff, divisor_floor}
#'          \item{sainte-lague, webster, divisor_round}
#'          \item{adams, divisor_ceiling}
#'          \item{dean, divisor_harmonic}
#'          \item{huntington-hill, hill-huntington, divisor_geometric}
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
    apport_methods = list(
        "d'hondt" = divisor_floor,
        "jefferson" = divisor_floor,
        "sainte-lague" = divisor_round,
        "webster" = divisor_round,
        "adams" = divisor_ceiling,
        "dean" = divisor_harmonic,
        "huntington-hill" = divisor_geometric,
        "hill-huntington" = divisor_geometric,
        "hare-niemeyer" = quota_largest_remainder,
        "hamilton" = quota_largest_remainder,
        "vinton" = quota_largest_remainder,
        "hagenbach-bischoff" = divisor_floor,
        "divisor_ceiling" = divisor_ceiling,
        "divisor_round" = divisor_round,
        "divisor_floor" = divisor_floor,
        "divisor_harmonic" = divisor_harmonic,
        "divisor_geometric" = divisor_geometric,
        "quota_largest_remainder" = quota_largest_remainder
    )
    method <- tolower(method)
    if(!method %in% names(apport_methods)) {
        stop("Unknown method: ", method, ".\nAvailable: ",
             paste0(names(apport_methods), collapse=", "))
    }

    apport_methods[[method]](votes, n_seats, quorum)
}

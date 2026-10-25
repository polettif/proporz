#' Check if parties reached the quorum for all votes
#'
#' Base implementation, used by \code{\link[=quorum_functions]{quorum_functions}}.
#'
#' @param votes_matrix Votes matrix
#' @param quorum_total Vote threshold a party (row) must reach for all votes cast.
#'   Used as fraction of total votes if less than 1, otherwise as number of votes.
#'   Must be greater than 0.
#'
#' @returns Logical vector with length equal to the number of lists/parties (`votes_matrix`
#'   rows) showing whether they reached the quorum or not.
#'
#' @note
#' Votes are not weighted across districts. This is relevant if the quorum threshold is the
#' minimal number of *voters* (either as percentage or absolute value). In this case, use
#' [weight_votes_matrix()] before calculating the quorum.
#'
#' @seealso [reached_quorum_any_district()]
#'
#' @examples
#' (vm <- matrix(c(239, 10, 308, 398, 20, 925), nrow = 3))
#' reached_quorum_total(vm, 35)
#' @export
reached_quorum_total = function(votes_matrix, quorum_total) {
    assert_num1(quorum_total)
    assert(quorum_total > 0)
    if(quorum_total < 1) {
        quorum_total <- sum(votes_matrix) * quorum_total
    }

    passed_total_quor = rowSums(votes_matrix) >= quorum_total
    return(passed_total_quor)
}

#' Check if parties reached a quorum in at least one district
#'
#' Base implementation, used by \code{\link[=quorum_functions]{quorum_functions}}.
#'
#' @param votes_matrix Votes matrix
#' @param quorum_districts Vote threshold a party (row) must reach in \emph{at least} one district.
#'   Used as fraction of total votes within a district if less than 1, otherwise as number of votes.
#'   Must be greater than 0.
#'
#' @inherit reached_quorum_total return
#' @seealso [reached_quorum_total()]
#'
#' @examples
#' (vm <- matrix(c(239, 10, 308, 398, 20, 925), nrow = 3))
#' reached_quorum_any_district(vm, 25)
#' @export
reached_quorum_any_district = function(votes_matrix, quorum_districts) {
    assert_num1(quorum_districts)
    assert(quorum_districts > 0)
    if(quorum_districts < 1) {
        quorum_districts <- colSums(votes_matrix) * quorum_districts
    } else {
        quorum_districts <- rep(quorum_districts, ncol(votes_matrix))
    }

    passed_distr_quor = votes_matrix >= row_as_matrix(quorum_districts, votes_matrix)
    passed_any_distr_quor = rowSums(passed_distr_quor) > 0
    return(passed_any_distr_quor)
}

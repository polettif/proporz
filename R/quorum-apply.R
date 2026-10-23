
# apply quorum functions to votes ####

#' Apply quorum to votes vector or matrix
#'
#' This quorum calculation implementation is called within [proporz()], [biproporz()] and
#' related functions. Generally, there is no need to call `apply_quorum` directly.
#'
#' @param votes Votes vector or votes matrix
#' @param quorum Depending on `votes`:
#'   * For a vector: Vote threshold a party must reach. Used as fraction of total
#'   votes if less than 1 otherwise as number of votes.
#'   * For a matrix: List of quorum functions (created with \code{\link[=quorum_functions]{quorum_functions}})
#'   or a (named) logical vector with the same length as the number of `votes` rows.
#' @param ... arguments passed on to the `quorum_function` (only for votes matrix)
#'
#' @returns Vector or matrix with same dimension as `votes`. Parties that failed to reach the
#'   specified quorum have their votes set to zero.
#'
#' @seealso \code{\link[=quorum_functions]{quorum_functions}} for more matrix examples.
#'
#' @export
#'
#' @examples
#' # vector
#' (votes <- c(81, 9, 10))
#'
#' apply_quorum(votes, 10)
#'
#' apply_quorum(votes, .11)
#'
#' # matrix
#' (votes_matrix <- matrix(c(91, 9, 199, 1), nrow = 2))
#'
#' apply_quorum(votes_matrix, quorum_all(votes_total = 0.1))
#'
#' apply_quorum(votes_matrix, c(FALSE, TRUE))
#'
#' # named matrix needs a named vector
#' rownames(votes_matrix) <- c("A", "B")
#' quorum_vec = c(B = TRUE, A = FALSE)
#' apply_quorum(votes_matrix, quorum_vec)
apply_quorum = function(votes, quorum, ...) {
    # TODO deprecate/info
    assert(is.matrix(votes) || is.atomic(votes))
    if(is.matrix(votes)) {
        b = apply_quorum_params(votes_matrix = votes, quorum_params = quorum, ...)
        return(set_zero(votes, !b))
    }
    apply_quorum_vector(votes, quorum)
}

# quorum for single vector for proporz() methods
apply_quorum_vector = function(votes_vector, quorum) {
    check_votes_vector(votes_vector, deparse(substitute(votes)))
    check_quorum_param_vector(quorum)

    if(quorum < 1) {
        quorum = ceiling(sum(votes_vector) * quorum)
    }

    if(all(votes_vector < quorum)) {
        stop("No party reached the quorum", call. = FALSE)
    }

    votes_vector[votes_vector < quorum] <- 0
    return(votes_vector)
}

check_quorum_param_vector = function(quorum) {
    if(!(is_num1(quorum) && quorum >= 0)) {
        stop("Quorum parameter must be a single number >= 0",
             call. = FALSE)
    }
    invisible(TRUE)
}

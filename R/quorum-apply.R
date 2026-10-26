#' Apply quorum to votes vector or matrix
#'
#' This quorum calculation implementation is called within [proporz()], there's generally
#' no need to call it directly. [biproporz()] uses [eval_quorum()].
#'
#' @param votes Votes vector or votes matrix
#' @param quorum Depending on `votes`:
#'   * For a vector: Vote threshold a party must reach. Used as fraction of total
#'     votes if less than 1 otherwise as number of votes.
#'   * For a matrix: List of quorum parameters (created with \code{\link[=quorum_functions]{quorum_functions}})
#'     or a (named) logical vector with the same length as the number of `votes` rows.
#'     To prevent potential misapplications, using `total_voters` or `any_district_voters`,
#'     which might require a weighted votes matrix, is not permitted.
# TODO
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
apply_quorum = function(votes, quorum) {
    assert(is.matrix(votes) || is.vector(votes))
    if(is.vector(votes)) {
        return(apply_quorum_boolvector(votes, quorum))
    }

    assert_viable_quorum(quorum)
    if(is_quorum_paramlist(quorum)) {
        methods_using_weighted_mtx = names(quorum)[endsWith(names(quorum), "_voters")]
        if(length(methods_using_weighted_mtx) > 0) {
            stop("Cannot handle unweighted matrix with ",
                 num_word("method", "methods", methods_using_weighted_mtx), ": ",
                 toString(methods_using_weighted_mtx),
                 call. = FALSE) # TODO link to eval_quorum
        }
    }
    b = eval_quorum(votes_matrix = votes, quorum = quorum)
    return(zero_failed_quorums(votes, b))
}

# quorum for single vector for proporz() methods
apply_quorum_boolvector = function(votes_vector, quorum) {
    check_votes_vector(votes_vector, deparse(substitute(votes)))
    check_quorum_param_vector(quorum)

    if(quorum < 1) {
        quorum = sum(votes_vector) * quorum
    }

    b = votes_vector < quorum
    if(all(b)) {
        stop("No party reached the quorum", call. = FALSE)
    }

    votes_vector[b] <- 0
    return(votes_vector)
}

zero_failed_quorums = function(M, quorum_vector) {
    assert(is.matrix(M))
    assert(is.logical(quorum_vector))
    assert(length(quorum_vector) == nrow(M))
    M[!quorum_vector,] <- 0L
    M
}

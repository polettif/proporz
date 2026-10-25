# Create quorum input functions ####

#' Create quorum functions for biproportional apportionment
#'
#' `quorum_any()` and `quorum_all()` are used for the `quorum` parameter in
#' [biproporz()]/[pukelsheim()] and help describe how quorums should be
#' applied prior to seat distributions.
#'
#' @param votes_any_district Vote threshold a party must reach in \emph{at least} one district.
#'   Used as share of total votes within a district if less than 1, otherwise as number of votes.
#'   Must be > 0 if provided.
#'   Uses [reached_quorum_any_district()].
#' @param votes_total Vote threshold a party must reach for all votes cast.
#'   Used as share of total votes if less than 1, otherwise as number of votes.
#'   Note that votes are not weighted with [weight_votes_matrix()] across districts, the
#'   usual and theoretically sound application for a total quroum is `total_voters`.
#'   Must be > 0 if provided.
#'   Uses [reached_quorum_total()].
#' @param any_district_voters Weighted vote (voters) threshold a party must reach in \emph{at least} one district.
#'   Used as share of total weighted votes (voters) within a district if less than 1, otherwise as number of voters.
#'   Must be > 0 if provided.
#'   Uses [reached_quorum_any_district()] with a weighted votes matrix.
#' @param total_voters Vote threshold a party must reach for all weighted votes (voters).
#'   Used as share of total weighted votes (voters) if less than 1, otherwise as number of voters.
#'   Must be > 0 if provided.
#'   Uses [reached_quorum_total()] with a weighted votes matrix.
#' @param total Replaced by `total_voters` (recommended) or `votes_total`
#' @param any_district Replaced by `votes_any_district` (recommended) or `any_district_voters`
#'
#' @details There is a difference in how the two functions work. With `quorum_any`,
#'   \emph{at least one} quorum must be reached. With `quorum_all` \emph{all}
#'   (i.e. both) quorums must be reached. If you only use one parameter,
#'   `quorum_any()` and `quorum_all()` are identical.
#'
#' @returns List of functions. Each function, when called with `function(votes_matrix)`,
#'   returns a boolean vector with length equal to the number of lists/parties
#'   (`votes_matrix` rows). The vector shows whether a party has reached any/all
#'   quorums.
#'
#' @seealso [apply_quorum()] for standalone quorum calculations
#'
#' @examples
#' votes_matrix = matrix(c(5020, 550, 800, 100, 1040, 550, 0, 10), ncol = 2)
#' dimnames(votes_matrix) <- list(c("A", "B", "C", "D"), c("Z1", "Z2"))
#' seats = c(Z1 = 50, Z2 = 20)
#'
#' # use as parameter in biproporz or pukelsheim (general use case)
#' biproporz(votes_matrix, seats,
#'           quorum = quorum_any(votes_any_district = 0.1, total_voters = 20))
#'
#' biproporz(votes_matrix, seats,
#'           quorum = quorum_all(votes_any_district = 0.1, total_voters = 20))
#'
#' biproporz(votes_matrix, seats, quorum = quorum_any(votes_any_district = 0.1))
#'
#' biproporz(votes_matrix, seats, quorum = quorum_any(total_voters = 0.05))
#'
#' # the quorum parameter also accepts named vectors (e.g. calculated elsewhere)
#' biproporz(votes_matrix, seats,
#'           quorum = c(A = FALSE, B = TRUE, C = TRUE, D = TRUE))
#'
#' @name quorum_functions
NULL

#' @rdname quorum_functions
#' @export
quorum_all = function(votes_any_district, votes_total,
                      any_district_voters, total_voters,
                      total = NULL, any_district = NULL) {
    catch_replaced_quorum_params(total, any_district)
    rm(total, any_district)

    quorum_params = c(as.list(environment()))
    quorum_params <- quorum_params[!sapply(quorum_params, is.symbol)]
    check_quorum_params_num1(quorum_params)

    attributes(quorum_params)[["type"]] <- "ALL"
    return(quorum_params)
}

#' @rdname quorum_functions
#' @export
quorum_any = function(votes_any_district, votes_total,
                      any_district_voters, total_voters,
                      total = NULL, any_district = NULL) {
    catch_replaced_quorum_params(total, any_district)
    rm(total, any_district)

    quorum_params = c(as.list(environment()))
    quorum_params <- quorum_params[!sapply(quorum_params, is.symbol)]
    check_quorum_params_num1(quorum_params)

    attributes(quorum_params)[["type"]] <- "ANY"
    return(quorum_params)
}

eval_quorum = function(quorum_params, votes_matrix, weighted_votes_matrix, ...) {
    assert_empty_dots(list(...))
    assert_viable_quorum(quorum_params)

    if(is_quorum_param_list(quorum_params)) {
        quorum_bool = eval_quorum_params_list(quorum_params, votes_matrix, weighted_votes_matrix, ...)
    } else if(is_quorum_vector(quorum_params)) {
        quorum_bool = eval_quorum_vector(quorum_params, votes_matrix)
    }

    return(quorum_bool)
}

eval_quorum_params_list = function(quorum_params, votes_matrix, weighted_votes_matrix, ...) {
    ql = list()

    # votes_
    if(!is.null(quorum_params$votes_any_district)) {
        if(missing(votes_matrix)) missing_arg_error("votes_matrix", "votes_any_district")
        votes_matrix <- prep_votes_matrix(votes_matrix, deparse(substitute(votes_matrix)))
        ql[[length(ql)+1]] <- reached_quorum_any_district(votes_matrix, quorum_params$votes_any_district)
    }

    if(!is.null(quorum_params$votes_total)) {
        if(missing(votes_matrix)) missing_arg_error("votes_matrix", "votes_total")
        votes_matrix <- prep_votes_matrix(votes_matrix, deparse(substitute(votes_matrix)))
        ql[[length(ql)+1]] <- reached_quorum_total(votes_matrix, quorum_params$votes_total)
    }

    # _voters
    if(!is.null(quorum_params$any_district_voters)) {
        if(missing(weighted_votes_matrix)) missing_arg_error("weighted_votes_matrix", "any_district_voters")
        weighted_votes_matrix <- prep_votes_matrix(weighted_votes_matrix, deparse(substitute(weighted_votes_matrix)))
        ql[[length(ql)+1]] <- reached_quorum_any_district(weighted_votes_matrix, quorum_params$any_district_voters)
    }

    if(!is.null(quorum_params$total_voters)) {
        if(missing(weighted_votes_matrix)) missing_arg_error("weighted_votes_matrix", "total_voters")
        weighted_votes_matrix <- prep_votes_matrix(weighted_votes_matrix, deparse(substitute(weighted_votes_matrix)))
        ql[[length(ql)+1]] <- reached_quorum_total(weighted_votes_matrix, quorum_params$total_voters)
    }

    quorum_matrix = do.call(cbind, ql)

    if(attr(quorum_params, "type") == "ALL") {
        quorum_bool = rowSums(quorum_matrix) == ncol(quorum_matrix)
    } else if(attr(quorum_params, "type") == "ANY") {
        quorum_bool = rowSums(quorum_matrix) > 0
    }
}

eval_quorum_vector = function(quorum_params, votes_matrix) {
    assert(length(quorum_params) == nrow(votes_matrix))
    if(!equal_names(rownames(votes_matrix), names(quorum_params))) {
        stop("quorum vector must have the same names as the votes_matrix rows (parties)",
             call. = FALSE)
    }
    if(!is.null(rownames(votes_matrix))) {
        quorum_bool = quorum_params[rownames(votes_matrix)]
    } else {
        quorum_bool = quorum_params
    }
}

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
#' votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)
#' dimnames(votes_matrix) <- list(c("A", "B", "C", "D"), c("Z1", "Z2"))
#' seats = c(Z1 = 50, Z2 = 20)
#'
#' # use as parameter in biproporz or pukelsheim (general use case)
#' biproporz(votes_matrix, seats,
#'           quorum = quorum_any(votes_any_district = 0.1, votes_total = 100))
#'
#' biproporz(votes_matrix, seats,
#'           quorum = quorum_all(votes_any_district = 0.1, votes_total = 100))
#'
#' biproporz(votes_matrix, seats, quorum = quorum_any(votes_any_district = 0.1))
#'
#' biproporz(votes_matrix, seats, quorum = quorum_any(votes_total = 100))
#'
#' biproporz(votes_matrix, seats, quorum = quorum_any(votes_total = 0.5))
#'
#' # the quorum parameter also accepts named vectors (e.g. calculated elsewhere)
#' biproporz(votes_matrix, seats,
#'           quorum = c(A = FALSE, B = TRUE, C = TRUE, D = TRUE))
#'
#' @name quorum_functions
NULL

#' @rdname quorum_functions
#' @export
quorum_all = function(votes_any_district,
                      votes_total,
                      any_district_voters,
                      total_voters,
                      total = NULL, any_district = NULL) {
    catch_replaced_quorum_params(total, any_district)
    rm(total, any_district)

    quorum_params = c(as.list(environment()))
    quorum_params <- quorum_params[!sapply(quorum_params, is.symbol)]

    for(q_name in names(quorum_params)) {
        if(!is_num1(quorum_params[[q_name]]) || quorum_params[[q_name]] <= 0) {
            stop("`", q_name, "` must be a single number > 0", call. = FALSE)
        }
    }

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

    # TODO explicit names?
    quorum_params = c(as.list(environment()))
    quorum_params <- quorum_params[!sapply(quorum_params, is.symbol)]

    for(q_name in names(quorum_params)) {
        if(!is_num1(quorum_params[[q_name]]) || quorum_params[[q_name]] <= 0) {
        stop("`", q_name, "` must be a single number > 0", call. = FALSE)

        }
    }

    attributes(quorum_params)[["type"]] <- "ANY"
    return(quorum_params)
}

# TODO rename to reached_quorum_...
apply_quorum_params = function(quorum_params, votes_matrix, weighted_votes_matrix) {
    if(is_quorum_param_list(quorum_params)) {
        # quorum params list
        ql = list()
        if(!is.null(quorum_params$votes_any_district)) {
            ql[[length(ql)+1]] <- reached_quorum_any_district(votes_matrix, quorum_params$votes_any_district)
        }
        if(!is.null(quorum_params$votes_total)) {
            ql[[length(ql)+1]] <- reached_quorum_total(votes_matrix, quorum_params$votes_total)
        }
        if(!is.null(quorum_params$any_district_voters)) {
            # TODO: error if weighted_votes matrix was not supplied
            ql[[length(ql)+1]] <- reached_quorum_any_district(weighted_votes_matrix, quorum_params$any_district_voters)
        }
        if(!is.null(quorum_params$total_voters)) {
            ql[[length(ql)+1]] <- reached_quorum_total(weighted_votes_matrix, quorum_params$total_voters)
        }
        quorum_matrix = do.call(cbind, ql)

        if(attr(quorum_params, "type") == "ALL") {
            quorum_bool = rowSums(quorum_matrix) == ncol(quorum_matrix)
        } else if(attr(quorum_params, "type") == "ANY") {
            quorum_bool = rowSums(quorum_matrix) > 0
        }

    } else if(is_quorum_vector(quorum_params)) {
        # vector
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
    } else {
        stop("Quorum must be a logical vector or a quorum parameter list (see ?quorum_functions)",
             call. = FALSE)
    }
    return(quorum_bool)
}

is_quorum_param_list = function(.quorum) {
    quorum_names = c("votes_any_district", "votes_total", "any_district_voters", "total_voters")
    if(!is.list(.quorum) || is.data.frame(.quorum)) {
        return(FALSE)
    }
    if(!all(names(.quorum) %in% quorum_names)) {
        return(FALSE)
    }
    if(length(setdiff(c("ANY", "ALL"), attributes(.quorum)[["type"]])) != 1) {
        return(FALSE)
    }
    return(TRUE)
}

is_quorum_vector = function(.quorum) {
    if(!is.atomic(.quorum) || !is.logical(.quorum) || anyNA(.quorum)) {
        return(FALSE)
    }
    return(TRUE)
}

# can't catch total/any_district via ... because partial argument
# matching (total to votes_total) happens first, dots are not even evaluated
catch_replaced_quorum_params = function(total, any_district) {
    if(!is.null(total)) {
        stop("The argument `total` is deprecated, ",
             "please specify total_voters (recommended) or `votes_total`",
             call. = FALSE)
    }
    if(!is.null(any_district)) {
        stop("The argument `any_district` is deprecated, ",
             "please specify `votes_any_district` (recommended) or `any_district_voters`",
             call. = FALSE)
    }
}

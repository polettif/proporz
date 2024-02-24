#' Create quorum functions for biproportional apportionment
#'
#' `quorum_any()` and `quorum_all()` are used for the `quorum` parameter in
#' [biproporz()] or [pukelsheim()] and help describe how quorums should be
#' applied previous to seat distributions.
#'
#' @param any_district Vote threshold a party must reach in \emph{at least} one
#'   district. Used as share of total votes within a district if less than 1
#'   otherwise as number of votes. Must be greater than 0. Uses
#'   [reached_quorum_any_district()].
#' @param total Vote threshold a party must reach for all votes cast. Used as
#'   share of total votes if less than 1, otherwise as number of votes. Must be
#'   greater than 0. Uses [reached_quorum_total()].
#'
#' @details There's a difference in how the functions work. With `quroum_any`,
#'   \emph{at least one} quorum must be reached. With `quorum_all` \emph{all}
#'   (i.e. both) quorums must be reached. If you only use one parameter,
#'   `quorum_any()` and `quorum_all()` are identical.
#'
#' @returns a function which, when called with `function(votes_matrix)`, returns
#'   a boolean vector with length equal to the number of lists/parties
#'   (`votes_matrix` rows). The vector shows whether a party has reached any/all
#'   quorums.
#'
#' @examples
#' votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)
#' dimnames(votes_matrix) <- list(c("A", "B", "C", "D"), c("Z1", "Z2"))
#' seats = c(Z1 = 50, Z2 = 20)
#'
#' # use as parameter in biproporz or pukelsheim (general use case)
#' biproporz(votes_matrix, seats, quorum = quorum_any(any_district = 0.1, total = 100))
#'
#' biproporz(votes_matrix, seats, quorum = quorum_all(any_district = 0.1, total = 100))
#'
#' biproporz(votes_matrix, seats, quorum = quorum_any(any_district = 0.1))
#'
#' biproporz(votes_matrix, seats, quorum = quorum_any(total = 100))
#'
#' biproporz(votes_matrix, seats, quorum = quorum_any(total = 0.5))
#'
#' # the quorum parameter also accepts vectors (e.g. calculated elsewhere)
#' biproporz(votes_matrix, seats, quorum = c(FALSE, TRUE, TRUE, TRUE))
#'
#' @name quorum_functions
NULL

#' @rdname quorum_functions
#' @export
quorum_all = function(any_district, total) {
    create_quorum_function_list("ALL", any_district, total)
}

#' @rdname quorum_functions
#' @export
quorum_any = function(any_district, total) {
    create_quorum_function_list("ANY", any_district, total)
}

create_quorum_function_list = function(type, any_district, total) {
    stopifnot(type %in% c("ANY", "ALL"))
    quorum_funcs = list()

    if(!missing(any_district)) {
        quorum_funcs <- append(quorum_funcs, function(votes_matrix) {
            reached_quorum_any_district(votes_matrix, any_district)
        })
    }

    if(!missing(total)) {
        quorum_funcs <- append(quorum_funcs, function(votes_matrix) {
            reached_quorum_total(votes_matrix, total)
        })
    }

    attributes(quorum_funcs)$type <- type
    return(quorum_funcs)
}

#' Check if lists/parties have reached the quorum for all votes
#'
#' Base implementation, used by [quorum_any()] and [quorum_all()].
#'
#' @param votes_matrix votes matrix
#' @param quorum_total Vote threshold a party must reach for all votes cast.
#'                     Used as quota of total votes if less than 1, otherwise
#'                     as number of votes. Must be greater than 0.
#'
#' @returns boolean vector with length equal to the number of lists/parties
#'          (`votes_matrix` rows) whether they reached the quorum or not
#'
#' @seealso [reached_quorum_any_district()]
#'
#' @examples
#' (vm = matrix(c(239, 10, 308, 398, 20, 925), nrow = 3))
#' reached_quorum_total(vm, 35)
#' @export
reached_quorum_total = function(votes_matrix, quorum_total) {
    stopifnot(quorum_total > 0)
    if(quorum_total < 1) {
        quorum_total <- sum(votes_matrix)*quorum_total
    }

    passed_total_quor = rowSums(votes_matrix) >= quorum_total
    return(passed_total_quor)
}

#' Check if lists/parties have reached a quorum in at least one district
#'
#' Base implementation, used by [quorum_any()] and [quorum_all()].
#'
#' @param votes_matrix votes matrix
#' @param quorum_districts Vote threshold a party must reach in \emph{at least}
#'                         one district. Used as quota of total votes within a
#'                         district if less than 1 otherwise as number of votes.
#'                         Must be greater than 0.
#'
#' @inherit reached_quorum_total return
#' @seealso [reached_quorum_total()]
#' @examples
#' (vm = matrix(c(239, 10, 308, 398, 20, 925), nrow = 3))
#' reached_quorum_any_district(vm, 25)
#' @export
reached_quorum_any_district = function(votes_matrix, quorum_districts) {
    stopifnot(quorum_districts > 0)
    if(quorum_districts < 1) {
        quorum_districts <- colSums(votes_matrix)*quorum_districts
    }

    passed_distr_quor = t(apply(votes_matrix, 1, function(x) x >= quorum_districts))
    passed_any_distr_quor = rowSums(passed_distr_quor) > 0
    return(passed_any_distr_quor)
}

#' Apply a list of quorum functions to a votes matrix
#'
#' @param votes_matrix votes matrix
#' @param quorum_funcs List of quorum functions. If list, the attribute "type"
#'                     must be set which indicates whether `ALL` or `ANY`
#'                     (i.e. at least one) quorum must be reached.
#'
#' This is a low-level implementation for quorum calculations and is
#' called within [biproporz()]. There's generally no need to call it
#' directly.
#'
#' @seealso [quorum_all()], [quorum_any()] to create a list of quorum functions.
#'
#' @inherit reached_quorum_total return
#' @examples
#' votes_matrix = matrix(c(502, 55, 80, 10, 104, 55, 0, 1), ncol = 2)
#'
#' quorum_functions = quorum_any(any_district = 0.1, total = 100)
#' proporz:::reached_quorums(votes_matrix, quorum_functions)
#' @keywords internal
reached_quorums = function(votes_matrix, quorum_funcs) {
    assert(is.matrix(votes_matrix))
    if(!is.list(quorum_funcs) || !is.function(quorum_funcs[[1]])) {
        stop("`", deparse(substitute(quorum_funcs)),
             "` is not a list of functions.", call. = F)
    }

    # list of vector whether quorum was reached for each party
    has_reached_quorum = lapply(quorum_funcs, function(qf) {
        qf(votes_matrix)
    })

    if(length(quorum_funcs) == 1) {
        return(quorum_funcs[[1]](votes_matrix))
    } else if(is.null(attributes(quorum_funcs)$type)) {
        stop("type must be set as list attribute.", call. = F)
    }

    quorum_matrix = do.call(cbind, has_reached_quorum)
    if(attributes(quorum_funcs)$type == "ALL") {
        quorum_bool = apply(quorum_matrix, 1, all)
    } else if(attributes(quorum_funcs)$type == "ANY") {
        quorum_bool = apply(quorum_matrix, 1, any)
    } else {
        stop("Unknown type `", attributes(quorum_funcs)$type, "`.", call. = F)
    }
    return(quorum_bool)
}

# quorum has to be a list of functions created by quorum_all/any because
# functions like reached_any_district need the threshold as parameter which
# won't work with only votes_matrix. So it's easier to simply restrict params
# and hopefully reduce misunderstandings
apply_quorum_matrix = function(votes_matrix, quorum) {
    assert(is.matrix(votes_matrix))
    if(is.list(quorum)) {
        quorum_bool = reached_quorums(votes_matrix, quorum)
    } else if(is.vector(quorum) && is.logical(quorum)) {
        stopifnot(length(quorum) == nrow(votes_matrix))
        quorum_bool = quorum
    } else {
        stop("Cannot parse quorum function or vector.", call. = F)
    }

    if(any(!quorum_bool)) {
        votes_matrix[!quorum_bool,] <- 0
    }

    return(votes_matrix)
}

# quorum for single vector for proporz() methods
apply_quorum_vector = function(votes_vector, quorum) {
    check_votes_vector(votes_vector)
    stopifnot(length(quorum) == 1, is.numeric(quorum), quorum >= 0)

    if(quorum < 1) {
        quorum = ceiling(sum(votes_vector)*quorum)
    }

    if(all(votes_vector < quorum)) {
        stop("No party reached the quorum.", call. = F)
    }

    votes_vector[votes_vector < quorum] <- 0
    return(votes_vector)
}

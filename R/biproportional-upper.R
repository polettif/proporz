#' Upper apportionment
#'
#' In the first step of biproportional apportionment parties are given seats according to the
#' sum of their votes across all districts.
#'
#' @param votes_matrix Vote count matrix with votes by party in rows and votes by district
#'   in columns.
#' @param district_seats
#'   - Vector defining the number of seats per district. Must be the same
#'   length as `ncol(votes_matrix)`. Values are name-matched to `votes_matrix` columns if both
#'   are named.
#'   - If the number of seats per district should be calculated according to the number
#'   of votes, a single number for the total number of seats can be used. `weight_votes` will be
#'   ignored, as `votes_matrix` shows the number of voters.
#'   Note that apportioning district seats according to votes is not the general use case.
#' @param weight_votes By default (`TRUE`) it is assumed that each voter in a district has
#'   as many votes as there are seats in a district. Thus, votes are weighted according to
#'   the number of available district seats with [weight_votes_matrix()]. Set to `FALSE` if
#'   `votes_matrix` shows the number of _voters_ (e.g. because they can only cast
#'   one vote for one party).
#' @param method Apportion method that defines how seats are assigned, see [proporz()]. Default
#'   is the Sainte-Laguë/Webster method.
#'
#' @seealso [biproporz()], [lower_apportionment()]
#'
#' @returns A named list with `district` seats (for `votes_matrix` columns) and `party` seats
#'   (for rows).
#'
#' @note The results from the upper apportionment define the number of seats for each party and
#'   the number of seats for each district for the whole voting area. The lower apportionment
#'   will only determine where (i.e. which district) the party seats are allocated. Thus, after
#'   the upper apportionment is done, the final strength of a party/district within the
#'   parliament is definite.
#'
#' @examples
#' votes_matrix = matrix(c(123,912,312,45,714,255,815,414,215), nrow = 3)
#' district_seats = c(7,5,8)
#'
#' upper_apportionment(votes_matrix, district_seats)
#'
#' @export
upper_apportionment = function(votes_matrix, district_seats,
                               quorum,
                               weight_votes = TRUE,
                               method = "round") {
    upp_app = list()
    # check parameters
    .vmn = deparse(substitute(votes_matrix))
    .dsn = deparse(substitute(district_seats))
    votes_matrix <- prep_votes_matrix(votes_matrix, .vmn)
    district_seats <- prep_district_seats(district_seats, votes_matrix, .dsn, .vmn)
    assert_bool1(weight_votes)

    # catch logical quorum as it is in the old/deprecated position of weight_votes
    if(!missing(quorum) && is_bool1(quorum) && nrow(votes_matrix) > 1) {
        stop("`quorum` cannot be TRUE/FALSE.\n",
             "Argument order has changed, do you need to set `weight_votes=`",
             quorum, "` explicitly?", call. = FALSE)
    }

    # apportion district seats
    if(length(district_seats) == 1L) {
        seats_district = proporz(colSums(votes_matrix), district_seats, method)
        weight_votes <- FALSE
    } else {
        assert(length(district_seats) == ncol(votes_matrix))
        seats_district = district_seats
    }

    # weight votes
    weighted_votes_matrix = weight_votes_matrix(votes_matrix, seats_district)

    # quorum
    if(!missing(quorum) && !is.null(quorum)) {
        party_reached_quorum = apply_quorum_matrix(votes_matrix, quorum, weighted_votes_matrix, TRUE)
    } else {
        party_reached_quorum = rep(TRUE, nrow(votes_matrix))
    }

    # define relevant matrix, apply quorum
    V = votes_matrix
    if(weight_votes) {
        V <- weighted_votes_matrix
    }
    V[!party_reached_quorum,] <- 0

    # check enough votes in districts and vice-versa
    # TODO separate function
    .novotes_check = colSums(V)[seats_district > 0] == 0
    if(any(.novotes_check)) {
        stop("No votes in ", num_word("a district", "districts", .novotes_check),
             " with at least one seat: ",
             collapse_names(.novotes_check, names(.novotes_check)),
             call. = FALSE)
    }

    # apportion party seats
    seats_party = proporz(rowSums(V), sum(seats_district), method)

    # return values
    upp_app = list(
        district = seats_district,
        party = seats_party)

    attributes(upp_app)$votes_matrix <- votes_matrix
    attributes(upp_app)$weighted_votes_matrix <- weighted_votes_matrix
    attributes(upp_app)$V <- V # relevant (weighted/unweighted) matrix, after quorum application

    return(upp_app)
}

#' Create weighted votes matrix
#'
#' Weight votes by dividing the votes matrix entries by the number
#' of seats per district. Votes in districts without seats are set
#' to zero.
#'
#' @seealso This method is used in [upper_apportionment()] if
#' `weight_votes` is `TRUE` (default).
#'
#' @param votes_matrix Votes matrix
#' @param district_seats Seats per district, vector with same length
#'   as `ncol(votes_matrix)` and names as `colnames(votes_matrix)`
#'
#' @note `weight_list_votes()` has been renamed to [weight_votes_matrix()]
#' in `v1.5.2` and is deprecated.
#'
#' @returns The weighted `votes_matrix` with the number of voters (not rounded).
#'
#' @examples
#' (vm <- matrix(c(20, 10, 8, 5), nrow = 2))
#' weight_votes_matrix(vm, c(3, 2))
#'
#' weight_votes_matrix(uri2020$votes_matrix, uri2020$seats_vector)
#'
#' @name weight_votes_matrix
NULL

#' @rdname weight_votes_matrix
#' @export
weight_votes_matrix = function(votes_matrix, district_seats) {
    votes_matrix <- prep_votes_matrix(votes_matrix, deparse(substitute(votes_matrix)))
    district_seats <- prep_district_seats(district_seats, votes_matrix, "district_seats", "votes_matrix")
    assert(all(district_seats >= 0))
    if(ncol(votes_matrix) != length(district_seats)) {
        stop("`length(district_seats)` must be the same as `ncol(votes_matrix)`", call. = FALSE)
    }

    M_seats_district = matrix(
        rep(district_seats, nrow(votes_matrix)),
        byrow = TRUE, ncol = length(district_seats))

    # it's possible if district seats are proportionally assigned that
    # a district has 0 seats, fix NaNs and Infs here
    votes_matrix <- div0(votes_matrix, M_seats_district)

    return(votes_matrix)
}

#' @rdname weight_votes_matrix
#' @export
weight_list_votes = function(votes_matrix, district_seats) {
    .Deprecated(
        new = "weight_votes_matrix",
        msg = "weight_list_votes has been renamed to weight_votes_matrix")
    weight_votes_matrix(votes_matrix, district_seats)
}

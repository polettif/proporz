#' Upper apportionment
#'
#' In the first step of biproportional apportionment parties are given seats according to the
#' sum of their votes across all districts.
#'
#' @param votes_matrix Vote count matrix with votes by party in rows and votes by district
#'   in columns.
#' @param district_seats Vector defining the number of seats per district. Must be the same
#'   length as `ncol(votes_matrix)`. Values are name-matched to `votes_matrix` columns if both
#'   are named. If the number of seats per district should be calculated according to the number
#'   of votes (not the general use case), a single number for the total number of seats can be
#'   used.
#' @param use_list_votes By default (`TRUE`) it's assumed that each voter in a district has
#'   as many votes as there are seats in a district. Thus, votes are weighted according to
#'   the number of available district seats with [weight_list_votes()]. Set to `FALSE` if
#'   `votes_matrix` shows the number of voters (i.e. they can only cast one vote for one
#'   party).
#' @param method Apportion method that defines how seats are assigned, see [proporz()]. Default
#'   is the Saintë-Lague/Webster method.
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
                               use_list_votes = TRUE,
                               method = "round") {
    # check parameters
    .votes_matrix.name = deparse(substitute(votes_matrix))
    .district_seats.name = deparse(substitute(district_seats))
    votes_matrix <- prep_votes_matrix(votes_matrix, .votes_matrix.name)
    district_seats <- prep_district_seats(district_seats, votes_matrix, .district_seats.name, .votes_matrix.name)
    assert(length(use_list_votes) == 1 && is.logical(use_list_votes))

    # district seats
    if(length(district_seats) == 1) {
        seats_district = proporz(colSums(votes_matrix), district_seats, method)
    } else {
        assert(length(district_seats) == ncol(votes_matrix))
        seats_district = district_seats
    }

    # party seats
    if(use_list_votes) {
        votes_matrix <- weight_list_votes(votes_matrix, seats_district)
    }
    seats_party = proporz(rowSums(votes_matrix), sum(seats_district), method)

    # check enough votes in districts
    if(!identical(colSums(votes_matrix) > 0, seats_district > 0)) {
        stop("No votes in a district with at least one seat", call. = FALSE)
    }

    # return values
    list(district = seats_district, party = seats_party)
}

#' Create weighted votes matrix
#'
#' Weight list votes by dividing the votes matrix entries by the number
#' of seats per district. This method is used in [upper_apportionment()] if
#' `use_list_votes` is `TRUE` (default).
#'
#' @param votes_matrix votes matrix
#' @param district_seats seats per district, vector with same length
#'   as `ncol(votes_matrix)`
#'
#' @note The weighted votes are not rounded. Matrix and vector names are ignored.
#'
#' @returns the weighted `votes_matrix`
#'
#' @examples
#' weight_list_votes(uri2020$votes_matrix, uri2020$seats_vector)
#'
#' @export
weight_list_votes = function(votes_matrix, district_seats) {
    if(ncol(votes_matrix) != length(district_seats)) {
        stop("`length(district_seats)` must be the same as `ncol(votes_matrix)`", call. = FALSE)
    }
    M_seats_district = matrix(
        rep(district_seats, nrow(votes_matrix)),
        byrow = TRUE, ncol = length(district_seats))

    votes_matrix <- votes_matrix/M_seats_district

    # it's possible if district seats are proportionally assigned that
    # a district has 0 seats, fix NaNs and Infs here
    votes_matrix[is.nan(votes_matrix) | is.infinite(votes_matrix)] <- 0

    return(votes_matrix)
}

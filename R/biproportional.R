#' Biproportional apportionment
#'
#' Method to proportionally allocate seats among parties (or lists) and districts (or
#' entities, regions), thus bi-proportional.
#'
#' @details Each party nominates a candidate list for every district. The voters vote for
#' the parties of their district. The seat allocation is calculated in two steps:
#' \enumerate{
#' \item In the so called \code{\link[=upper_apportionment]{upper apportionment}}
#'    the number of seats for each party (over all districts) is determined.
#'    Normally, the number of seats for each region are defined before the
#'    election and are independent of the vote counts.
#' \item In the so called \code{\link[=lower_apportionment]{lower apportionment}}
#'    the seats are distributed to the regional party list respecting the
#'    results from the upper apportionment.
#' }
#'
#' Parties failing to reach quorums cannot get seats. This function does not handle seat
#' assignment to candidates.
#'
#' @inheritParams upper_apportionment
#' @param quorum Optional list of functions which take the votes_matrix and return a logical
#'   vector that denotes for each party/row whether they reached the quorum (i.e. are
#'   eligible for seats). The easiest way to do this is via [quorum_any()] or
#'   [quorum_all()], see examples. Alternatively you can pass a precalculated logical
#'   vector. No quorum is applied if parameter is missing or `NULL`.
#' @param method Defines which method is used to assign seats. The following methods are
#'   recommended:
#'   \itemize{
#'     \item{`round`: Uses the Sainte-Laguë/Webster method (rounding half up) for the upper
#'           and lower apportionment which is the standard for biproportional apportionment and
#'           the only method guaranteed to terminate.}
#'     \item{`wto`: "winner take one" works like "round" with a condition that the party that
#'           got the most votes in a district must get _at least_ one seat ('Majorzbedingung')
#'           in said district. This only applies if they got enough seats in the upper
#'           apportionment (which uses the Sainte-Laguë/Webster method). See
#'           [lower_apportionment()] for more details.}
#'   }
#'   It is also possible to use any divisor method name listed in [proporz()]. If you want to
#'   use a different method for the upper and lower apportionment, provide a list with two
#'   entries.
#'
#' @note The iterative process in the lower apportionment is only guaranteed to terminate
#'   with the default Sainte-Laguë/Webster method.
#'
#' @references Gaffke, Norbert; Pukelsheim, Friedrich (2008): Divisor methods for
#'   proportional representation systems: An optimization approach to vector and matrix
#'   apportionment problems. Mathematical Social Sciences, 56 (2), 166-184.
#'
#' @seealso [pukelsheim()] for biproportional apportionment with `data.frames` as inputs.
#'
#' @returns Matrix with the same dimension as `votes_matrix` containing the number of seats
#'   with the row and column divisors stored in attributes (hidden from print, see
#'   [get_divisors()]).
#'
#' @examples
#' votes_matrix = uri2020$votes_matrix
#' district_seats = uri2020$seats_vector
#'
#' biproporz(votes_matrix, district_seats)
#'
#' # apply quorum (high values for illustrative purposes)
#' biproporz(votes_matrix, district_seats,
#'           quorum_all(any_district = 0.1, total = 0.25))
#'
#' @importFrom stats setNames
#' @export
biproporz = function(votes_matrix,
                     district_seats,
                     quorum,
                     use_list_votes = TRUE,
                     method = "round") {
    # check parameters
    .votes_matrix.name = deparse(substitute(votes_matrix))
    .district_seats.name = deparse(substitute(district_seats))
    votes_matrix <- prep_votes_matrix(votes_matrix, .votes_matrix.name)
    district_seats <- prep_district_seats(district_seats, votes_matrix, .district_seats.name, .votes_matrix.name)
    method <- prep_method(method)

    # quorum
    if(!missing(quorum) && !is.null(quorum)) {
        votes_matrix <- apply_quorum_matrix(votes_matrix, quorum)
    }

    # upper apportionment (Oberzuteilung)
    upp_app = upper_apportionment(votes_matrix, district_seats, use_list_votes, method[[1]])

    # lower apportionment (Unterzuteilung)
    seats_matrix = lower_apportionment(votes_matrix, upp_app$district, upp_app$party, method[[2]])

    class(seats_matrix) <- c("proporz_matrix", class(seats_matrix))
    return(seats_matrix)
}

#' Biproportional apportionment with data frames
#'
#' Method to proportionally allocate seats among parties/lists and
#' districts/regions/entities ('Doppelter Pukelsheim').
#'
#' Each party nominates a candidate list for every district. The voters vote for the parties
#' of their district. The seat allocation is calculated in two steps:
#'
#' \enumerate{
#'   \item In the so called \code{\link[=upper_apportionment]{upper apportionment}}
#'   the number of seats for each party (over all districts) is determined.
#'   \item In the so called \code{\link[=lower_apportionment]{lower apportionment}}
#'   the seats are distributed to the regional party list respecting the results
#'   from the upper apportionment.
#' }
#'
#' Parties failing to reach quorums cannot get seats. This function does not handle seat
#' assignment to candidates.
#'
#' If you want to use other apportion methods than Sainte-Laguë use [biproporz()].
#'
#' @param votes_df data.frame (long format) with 3 columns (actual colnames can differ):
#'                 \itemize{
#'                   \item party id/name
#'                   \item district id/name
#'                   \item votes
#'                   }
#' @param district_seats_df data.frame with 2 columns (actual colnames can differ):
#'                          \itemize{
#'                            \item district id/name
#'                            \item number of seats for a district
#'                          }
#' @inheritParams biproporz
#' @param new_seats_col name of the new column
#' @param use_list_votes By default (`TRUE`) it's assumed that each voter in a district has
#'   as many votes as there are seats in a district. Set to `FALSE` if `votes_df` shows the
#'   number of voters (e.g. they can only vote for one party).
#' @param winner_take_one Set to `TRUE` if the party that got the most votes in a district
#'   must get _at least_ one seat ('Majorzbedingung') in this district. This only applies if
#'   they are entitled to a seat in the upper apportionment. Default is `FALSE`.
#'
#' @seealso This function calls [biproporz()] after preparing the input data.
#'
#' @returns A data.frame like `votes_df` with a new column denoting the number seats per
#'   party and district. Party and district divisors stored in attributes in attributes
#'   (hidden from print, see [get_divisors()]).
#'
#' @examples
#' # Zug 2018
#' votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
#' district_seats_df = unique(zug2018[c("entity_id", "election_mandates")])
#'
#' seats_df = pukelsheim(votes_df,
#'                       district_seats_df,
#'                       quorum_any(any_district = 0.05, total = 0.03),
#'                       winner_take_one = TRUE)
#'
#' head(seats_df)
#'
#' # Finland 2019
#' finland19_result = pukelsheim(finland2019$votes_df,
#'                              finland2019$district_seats_df,
#'                              new_seats_col = "mandates",
#'                              use_list_votes = FALSE)
#' tail(finland19_result[order(finland19_result$mandates),])
#'
#' @export
pukelsheim = function(votes_df, district_seats_df,
                      quorum,
                      new_seats_col = "seats",
                      use_list_votes = TRUE,
                      winner_take_one = FALSE) {

    check_params.pukelsheim(votes_df, district_seats_df, new_seats_col, use_list_votes, winner_take_one,
                            deparse(substitute(votes_df)), deparse(substitute(district_seats_df)))

    # Create votes matrix
    votes_matrix = pivot_to_matrix(votes_df) # list_ids must be rows

    # "deframe" to named vector
    district_seats = prep_district_seats_df(district_seats_df)

    # Biproportional Apportionment
    method = ifelse(winner_take_one, "wto", "round")
    m = biproporz(votes_matrix, district_seats,
                  quorum = quorum,
                  use_list_votes = use_list_votes,
                  method = method)
    seats_df = pivot_to_df(m, new_seats_col)

    # join with original table
    assert(nrow(votes_df) <= nrow(seats_df))
    return_df = merge(votes_df,
                      seats_df,
                      sort = FALSE,
                      by = colnames(votes_df)[1:2])
    class(return_df) <- class(votes_df)
    attributes(return_df)$divisors <- attributes(m)$divisors
    return(return_df)
}

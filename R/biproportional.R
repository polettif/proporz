#' Biproportional apportionment with data frames
#'
#' Method to proportionally allocate seatsamong parties/lists and
#' districts/regions/entities ("Doppelter Pukelsheim").
#'
#' Each party nominates a candidate list for every district. The voters vote
#' for the parties of their district. The seat allocation is calculated in two
#' steps:
#'
#' \enumerate{
#'   \item In the so called \emph{upper apportionment} the number of seats for
#'   each party (over all districts) is determined.
#'   \item In the so called \emph{lower apportionment} the seats are distributed
#'   to the regional party list respecting the results from the upper apportionment.
#' }
#'
#' Parties failing to reach quorums cannot get seats.
#'
#' If you want to other apportion methods than Sainte-Laguë use \link{biproportional}.
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
#' @inheritParams biproportional
#' @param new_seats_col name of the new column
#' @param use_list_votes By default (TRUE) it's assumed that each voter in a district has as
#'                       many votes as there are seats in a district. Set to FALSE if
#'                       \code{votes_df} shows the number of voters.
#'
#' @seealso \code{\link{biproportional}}, \code{\link{divisors}}
#'
#' @examples
#' # Zug 2018
#' votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
#' district_seats_df = unique(zug2018[c("entity_id", "election_mandates")])
#'
#' seats_df = pukelsheim(votes_df,
#'                       district_seats_df,
#'                       quorum_any(any_district = 0.05, total = 0.03))
#'
#' head(seats_df)
#' #>   list_id entity_id list_votes seats
#' #> 1       2      1701       8108     2
#' #> 2       1      1701       2993     0
#' #> 3       3      1701      19389     3
#' #> 4       4      1701      14814     2
#' #> 5       5      1701       4486     1
#' #> 6       6      1701      15695     3
#'
#' # Finland 2019
#' finland19_result = pukelsheim(finland2019$votes_df,
#'                              finland2019$district_seats_df,
#'                              new_seats_col = "mandates",
#'                              use_list_votes = FALSE)
#' tail(finland19_result[order(finland19_result$mandates),])
#' #> list_id entity_id list_votes mandates
#' #>    VIHR       HEL      90662        5
#' #>      PS       UUS      86691        5
#' #>    VIHR       UUS      73626        5
#' #>    KESK       OUL      78486        6
#' #>     SDP       UUS      97107        6
#' #>     KOK       UUS     114243        7
#'
#' @md
#' @export
pukelsheim = function(votes_df, district_seats_df,
                      quorum,
                      new_seats_col = "seats",
                      use_list_votes = TRUE) {

    check_params.pukelsheim(votes_df, district_seats_df, new_seats_col, use_list_votes,
                            deparse(substitute(votes_df)), deparse(substitute(district_seats_df)))

    # Create votes matrix
    votes_matrix = pivot_to_matrix(votes_df) # list_ids must be rows

    # "deframe" to named vector
    ds_df <- district_seats_df[order(district_seats_df[[1]]),]
    district_seats <- ds_df[[2]]
    names(district_seats) <- ds_df[[1]]

    # Biproportional Apportionment
    m = biproportional(votes_matrix, district_seats,
                       quorum = quorum,
                       use_list_votes = use_list_votes)
    seats_df = pivot_to_df(m, new_seats_col)

    stopifnot(nrow(votes_df) <= nrow(seats_df))

    return_df = merge(votes_df,
                      seats_df,
                      sort = FALSE,
                      by = colnames(votes_df)[1:2])
    class(return_df) <- class(votes_df)
    attributes(return_df)$divisors <- attributes(m)$divisors
    return(return_df)
}

#' Biproportional apportionment
#'
#' Method to proportionally allocate seats among parties (or lists) and
#' districts (or entities, regions).
#'
#' Each party nominates a candidate list for every district. The voters vote
#' for the parties of their district. The seat allocation is calculated in two
#' steps:
#' \enumerate{
#' \item In the so called \emph{upper apportionment} the number of seats for
#'    each party (over all districts) is determined. Normally, the number of
#'    seats for each region are defined before the election and are independent
#'    of the vote counts.
#' \item In the so called \emph{lower apportionment} the seats are distributed
#'    to the regional party list respecting the results from the upper
#'    apportionment.
#' }
#'
#' Parties failing to reach quorums cannot get seats.
#'
#' @inheritParams upper_apportionment
#' @param quorum Optional list of functions which take the votes_matrix and
#'               return a logical vector that denotes for each list/party
#'               whether they reached the quorum (i.e. are elegible for seats).
#'               The easiest way to do this is via [quorum_any()] or
#'               [quorum_all()], see examples. Alternatively you can pass a
#'               precalculated logical vector. No quorum is applied if parameter
#'               is missing or NULL.
#' @param method Defines how seats in upper and lower apportionment are
#'               assigned. The default "round" for the Sainte-Laguë/Webster
#'               method is the standard for biproportional apportionment. See
#'               \link{proporz} for a list of available methods. For a different
#'               method for upper and lower apportionment use a vector with two
#'               entries. It is also possible to provide a function that works
#'               like base::round(x) (i.e. can handle a matrix).
#'
#' @note The iterative process in the lower apportionment is only guaranteed to terminate
#'       with Sainte-Laguë/Webster method.
#'
#' @seealso \code{\link{pukelsheim}} for usage with data frames.
#'          \code{\link{divisors}} to access the divisors
#'
#' @examples
#' votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
#' votes_matrix = pivot_to_matrix(votes_df)
#' votes_matrix
#' #>        entity_id
#' #> list_id  1701 1702 1703 1704 1705 1706 1707 1708 1709 1710  1711
#' #>       1  2993    0    0    0    0    0    0    0    0    0     0
#' #>       2  8108 4687 1584  531  279  477 2363 3860 1481   91 22023
#' #>       3 19389 9334 4807 1946  396 2844 3523 4702 3310  812 21343
#' #>       4 14814 6691 4005  826  379 1654 2842 2624 2713  461 33789
#' #>       5  4486 2270  621  198    0  361  728  465  925    0 10131
#' #>       6 15695 4705 1750   84    0   51  627 1106 1563  302 21794
#' #>       7 21298 8178 2875 1336  399 1450 3715 2610 4063  344 26798
#'
#' distr_df = unique(zug2018[c("entity_id", "election_mandates")])
#' district_seats = setNames(distr_df$election_mandates, distr_df$entity_id)
#' district_seats
#' #> 1701 1702 1703 1704 1705 1706 1707 1708 1709 1710 1711
#' #>   15   10    6    3    2    4    7    6    6    2   19
#'
#' biproportional(votes_matrix, district_seats, quorum_any(0.05, 0.03))
#' #>         entity_id
#' #> list_id 1701 1702 1703 1704 1705 1706 1707 1708 1709 1710 1711
#' #>       1    0    0    0    0    0    0    0    0    0    0    0
#' #>       2    2    1    1    0    0    0    1    2    1    0    3
#' #>       3    3    3    2    1    1    2    2    2    1    1    3
#' #>       4    2    2    1    1    0    1    2    1    1    1    5
#' #>       5    1    1    0    0    0    0    0    0    0    0    2
#' #>       6    3    1    1    0    0    0    0    0    1    0    3
#' #>       7    4    2    1    1    1    1    2    1    2    0    3
#'
#' # calculate quorum beforehand (leads to the same result as above)
#' q1 = reached_quorum_any_district(votes_matrix, 0.05)
#' q2 = reached_quorum_total(votes_matrix, 0.03)
#' reached_quorum = q1 | q2
#' reached_quorum
#' #>     1     2     3     4     5     6     7
#' #> FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
#'
#' biproportional(votes_matrix, district_seats, reached_quorum)
#'
#' # Different method for upper apportionment
#' # and using number of voters instead of list votes
#' f19_matrix = pivot_to_matrix(finland2019$votes_df)
#' f19_distr_seats = setNames(
#'     finland2019$district_seats_df$election_mandates,
#'     finland2019$district_seats_df$entity_id)
#'
#' f19_seats = biproportional(f19_matrix, f19_distr_seats,
#'                use_list_votes = FALSE,
#'                method = c("floor", "round"))
#'
#' f19_seats[rowSums(f19_seats) > 0,]
#' #>        entity_id
#' #> list_id HAEM HEL KAA KES LAP OUL PIR SAT SKA UUS VAA VAR
#' #> KD      1   0   1   1   0   1   1   0   1   1   1   0
#' #> KESK    1   1   3   2   2   6   2   1   3   2   3   2
#' #> KOK     3   5   3   1   1   2   4   1   2   7   2   4
#' #> Nyt     0   0   0   0   0   0   0   0   0   1   0   0
#' #> NYT     0   1   0   0   0   0   0   0   0   0   0   0
#' #> PIR     0   1   0   0   0   0   0   0   0   0   0   0
#' #> PS      3   3   3   2   1   4   3   2   3   6   3   3
#' #> RKP     0   1   0   0   0   0   0   0   0   4   3   1
#' #> SDP     4   3   4   2   1   2   4   2   3   7   2   3
#' #> SIN     0   0   0   0   0   0   0   0   1   1   0   0
#' #> VAS     1   2   1   1   1   2   2   1   1   2   1   2
#' #> VIHR    1   5   2   1   1   1   3   1   1   5   1   2
#'
#'
#' @importFrom stats setNames
#' @md
#' @export
biproportional = function(votes_matrix,
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
        votes_matrix <- apply_quorum(votes_matrix, quorum)
    }

    # upper apportionment (Oberzuteilung)
    upp_app = upper_apportionment(votes_matrix, district_seats, use_list_votes, method[1])

    # lower apportionment (Unterzuteilung)
    seats_matrix = lower_apportionment(votes_matrix, upp_app$district, upp_app$party, method[2])

    class(seats_matrix) <- c("proporz_matrix", class(seats_matrix))
    return(seats_matrix)
}

#' @inherit biproportional
#' @export
biproporz = biproportional

#' Calculate upper apportionment
#'
#' In the upper apportionment, the seats for each party are computed with a
#' highest averages method. This determines how many of all seats each party
#' deserves due to the total of all their votes (that is the sum of the votes
#' for all regional lists of that party). Analogical, the same highest averages
#' method is used to determine how many of all seats each region deserves.
#'
#' @note The results from the upper apportionment are final results for the
#' number of the seats of one party (and analogically for the number of the
#' seats of one region) within the whole voting area, the lower apportionment
#' will only determine where (which regions) the party seats are
#' allocated. Thus, after the upper apportionment is done, the final strength of
#' a party/region within the parliament is definite.
#'
#' @param votes_matrix Vote count matrix with votes by party in rows
#'                     and votes by district in columns
#' @param district_seats Vector defining the number of seats per district.
#'                       Must be the same length as ncol(votes_matrix). If the
#'                       number of seats per district should be assigned
#'                       according to the number of votes (not the general use
#'                       case), a single number for the total number of seats
#'                       can be used.
#' @param use_list_votes By default (TRUE) it's assumed that each voter in a
#'                       district has as many votes as there are seats in a
#'                       district. Set to FALSE if \code{votes_matrix} shows the
#'                       number of voters.
#' @param method Apportion method that defines how seats are assigned,
#'               see \link{proporz}.
#'
#' @seealso \link{biproportional}, \link{lower_apportionment}
#'
#' @return named list with column/district seats and row/party seats
#' @export
upper_apportionment = function(votes_matrix, district_seats,
                               use_list_votes = TRUE,
                               method = "round") {
    stopifnot(is.matrix(votes_matrix), is.logical(use_list_votes))
    # district seats
    if(length(district_seats) == 1) {
        seats_district = proporz(colSums(votes_matrix), district_seats, method)
    } else {
        seats_district = district_seats
    }

    # party seats
    if(use_list_votes) {
        M_seats_district = matrix(
            rep(seats_district, nrow(votes_matrix)),
            byrow = TRUE, ncol = length(seats_district))

        votes_matrix <- votes_matrix/M_seats_district

        # it's possible if district seats are proportionally assigned that
        # a district has 0 seats, fix NaNs and Infs here
        votes_matrix[is.nan(votes_matrix) | is.infinite(votes_matrix)] <- 0
    }
    seats_party = proporz(rowSums(votes_matrix), sum(seats_district), method)

    # check enough votes in districts
    if(!identical(colSums(votes_matrix) > 0, seats_district > 0)) {
        stop("No votes in a district with at least one seat", call. = FALSE)
    }

    # return values
    list(district = seats_district, party = seats_party)
}

#' Calculate lower apportionment
#'
#' Iterate and change column and row divisors such that
#' \code{colSums(round(M/col_divisors/row_divisors)) == seats_col} and
#' \code{rowSums(round(M/col_divisors/row_divisors)) == seats_row}
#'
#' The result is obtained by an iterative process. Initially, for each district
#' a divisor is chosen using the highest averages method for the votes allocated
#' to each regional party list in this region. For each party a party divisor is
#' initialized with 1.
#'
#' Effectively, the objective of the iterative process is to modify the regional
#' divisors and party divisors so that the number of seats in each regional
#' party list equals the number of their votes divided by both the regional and
#' the party divisors.
#'
#' The following two correction steps are executed until this objective is
#' satisfied:
#' \itemize{
#'   \item modify the party divisors such that the apportionment within each
#'     party is correct with the chosen rounding method,
#'   \item modify the regional divisors such that the apportionment within the
#'     region is correct with the chosen rounding method.
#' }
#'
#' @param M vote matrix
#' @param seats_cols number of seats per column (districts/regions), calculated
#'                   from upper_apportionment()
#' @param seats_rows number of seats per row (parties/lists), calculated
#'                   from upper_apportionment()
#' @param method Apportion method that defines how seats are assigned,
#'               see \link{proporz}. Note  that the iterative process is only
#'               guaranteed to terminate with "round" (Sainte-Laguë/Webster
#'               method). It is also possible to provide a function that works
#'               like base::round(x) (i.e. can handle a matrix).
#'
#' @note The iterative process in the lower apportionment is only guaranteed to
#'       terminate with Sainte-Laguë/Webster method.
#'
#' @return seat matrix with column and row divisors stored in attributes
#'
#' @seealso \link{biproportional}, \link{upper_apportionment}
#'
#' @references Oelbermann, K. F. (2016). Alternate scaling algorithm for
#' biproportional divisor methods. Mathematical Social Sciences, 80, 25-32.
#'
#' @export
lower_apportionment = function(M, seats_cols, seats_rows, method = "round") {
    if(sum(M %% 1) != 0) stop("matrix must only contain integers")
    stopifnot((seats_cols %% 1) == 0,
              (seats_rows %% 1) == 0)
    stopifnot(length(seats_cols) == ncol(M))
    stopifnot(length(seats_rows) == nrow(M))

    if(is.function(method)) {
        round_func = method
    } else {
        method_name = get_apport_method(method)
        if(method_name != "round") {
            warning('Lower apportionment is only guaranteed to terminate with the default ',
                    'Sainte-Lagu\uEB/Webster method (method = "round")', call. = FALSE)
        }
        round_func = get_round_function(method)
    }

    # divisor parties
    dP = rep(1, nrow(M))
    dP.min = rep(0.5, nrow(M))
    dP.max = rep(1.5, nrow(M))
    # divisor districts
    dD = round(colSums(M)/seats_cols)
    dD.min = floor(colSums(M)/(seats_cols+1) / max(dP.max))
    dD.max = ceiling(colSums(M)/(seats_cols-1) / min(dP.min))

    # calculate raw seat matrix
    # accesses function environment variables div_distr and div_party
    m. = function(.M, .div_distr, .div_party) {
        M_district = matrix(rep(.div_distr, nrow(.M)), byrow = TRUE, nrow = nrow(.M))
        M_party = matrix(rep(.div_party, ncol(.M)), byrow = FALSE, nrow = nrow(.M))

        x = .M/M_district/M_party
        x[is.nan(x)] <- 0
        return(x)
    }

    # convenience functions to round and summarise
    mc = function(.M,.d,.p) colSums(round_func(m.(.M,.d,.p)))
    mr = function(.M,.d,.p) rowSums(round_func(m.(.M,.d,.p)))

    which.min0 = function(x) {
        x[which(x == 0)] <- max(x)
        if(all(x == max(x))) return(c())
        which.min(x)
    }
    which.max0 = function(x) {
        x[which(x == 0)] <- min(x)
        if(all(x == min(x))) return(c())
        which.max(x)
    }

    while(!all(c(mc(M,dD,dP) == seats_cols, mr(M,dD,dP) == seats_rows))) {
        # change party divisors
        row_decr = which.min0(mr(M,dD,dP) - seats_rows)
        if(length(row_decr) == 1) {
            dP[row_decr] <- find_divisor(
                M[row_decr,]/dD,
                dP[row_decr], dP.min[row_decr],
                seats_rows[row_decr])
        }

        row_incr = which.max0(mr(M,dD,dP) - seats_rows)
        if(length(row_incr) == 1) {
            dP[row_incr] <- find_divisor(
                M[row_incr,]/dD,
                dP[row_incr], dP.max[row_incr],
                seats_rows[row_incr])
        }

        # change district divisors
        col_decr = which.min0(mc(M,dD,dP) - seats_cols)
        if(length(col_decr) == 1) {
            dD[col_decr] <- find_divisor(
                M[,col_decr]/dP,
                dD[col_decr], dD.min[col_decr],
                seats_cols[col_decr])
        }

        col_incr = which.max0(mc(M,dD,dP) - seats_cols)
        if(length(col_incr) == 1) {
            dD[col_incr] <- find_divisor(
                M[,col_incr]/dP,
                dD[col_incr], dD.max[col_incr],
                seats_cols[col_incr])
        }
    }

    output = round(m.(M, dD, dP))
    attributes(output)$divisors <- list()
    attributes(output)$divisors$districts <- dD
    names(attributes(output)$divisors$districts) <- colnames(M)
    attributes(output)$divisors$parties <- dP
    names(attributes(output)$divisors$parties) <- rownames(M)
    return(output)
}

find_divisor = function(votes,
                        divisor_from, divisor_to,
                        target_seats) {
    stopifnot(length(target_seats) == 1)

    fun = function(divisor) {
        target_seats - sum(round(votes/divisor))
    }

    divisor_range = sort(c(divisor_from, divisor_to))

    if(any(is.infinite(votes)) || any(is.nan(votes))) {
        stop("Result is undefined", call. = FALSE)
    }

    # Divisors should be within votes/(seats-1) and votes/(seats+1).
    # It might be necessary to increase the search range given that
    # party divisors are applied as well
    while(fun(divisor_range[1]) > 0 && fun(divisor_range[2]) > 0) {
        # expand lower limit
        divisor_range[1] <- divisor_range[1]/2
    }
    while(fun(divisor_range[1]) < 0 && fun(divisor_range[2]) < 0) {
        # expand upper limit
        divisor_range[2] <- divisor_range[2]*2
    }

    bisect(fun, divisor_range[1], divisor_range[2])
}

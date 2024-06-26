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
#'   must get _at least_ one seat ('Majorzbedingung') in this district. Default is `FALSE`.
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
#'   vector that denotes for each list/party whether they reached the quorum (i.e. are
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
#'           in said district. Seats in the upper apportionment are assigned with
#'           Sainte-Laguë/Webster. `votes_matrix` must have row and column names to use this
#'           method. See [lower_apportionment()] for more details.}
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

#' Calculate upper apportionment
#'
#' In the upper apportionment, the seats for each party are computed with a highest averages
#' method. This determines how many of all seats each party deserves due to the total of all
#' their votes (that is the sum of the votes for all regional lists of that party).
#' Analogical, the same highest averages method is used to determine how many of all seats
#' each region deserves.
#'
#' @param votes_matrix Vote count matrix with votes by party in rows and votes by district
#'   in columns
#' @param district_seats Vector defining the number of seats per district. Must be the same
#'   length as `ncol(votes_matrix)`. Values are name-matched to `votes_matrix` if both are
#'   named. If the number of seats per district should be assigned according to the number
#'   of votes (not the general use case), a single number for the total number of seats can
#'   be used.
#' @param use_list_votes By default (`TRUE`) it's assumed that each voter in a district has
#'   as many votes as there are seats in a district. Thus, votes are weighted according to
#'   the number of available district seats with [weight_list_votes()]. Set to `FALSE` if
#'   `votes_matrix` shows the number of voters (e.g. they can only cast one vote for one
#'   party).
#' @param method Apportion method that defines how seats are assigned, see [proporz()]. Default
#'   is the Saintë-Lague/Webster method.
#'
#' @seealso [biproporz()], [lower_apportionment()]
#'
#' @returns A named list with district seats (for `votes_matrix` columns) and party seats
#'   (for rows).
#'
#' @note The results from the upper apportionment are final results for the number of the
#'   seats of one party (and analogically for the number of the seats of one region) within
#'   the whole voting area, the lower apportionment will only determine where (which
#'   regions) the party seats are allocated. Thus, after the upper apportionment is done,
#'   the final strength of a party/region within the parliament is definite.
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
#' Weigh list votes by dividing the votes matrix entries by the number
#' of seats per district. This method is used in [upper_apportionment()] if
#' `use_list_votes` is `TRUE` (default). The weighted votes are not rounded.
#'
#' @param votes_matrix votes matrix
#' @param seats_district seats per district, vector with same length
#'   as `ncol(votes_matrix)`)
#'
#' @returns the weighted `votes_matrix`
#'
#' @examples
#' weight_list_votes(uri2020$votes_matrix, uri2020$seats_vector)
#'
#' @export
weight_list_votes = function(votes_matrix, seats_district) {
    M_seats_district = matrix(
        rep(seats_district, nrow(votes_matrix)),
        byrow = TRUE, ncol = length(seats_district))

    votes_matrix <- votes_matrix/M_seats_district

    # it's possible if district seats are proportionally assigned that
    # a district has 0 seats, fix NaNs and Infs here
    votes_matrix[is.nan(votes_matrix) | is.infinite(votes_matrix)] <- 0

    return(votes_matrix)
}

#' Calculate lower apportionment
#'
#' Iterate and change column and row divisors such that the row and column sums of the seats
#' matrix satisfies the constraints given by the upper apportionment.
#'
#' The result is obtained by an iterative process ('Alternate Scaling Algorithm', see
#' Reference). Initially, for each district a divisor is chosen using the highest averages
#' method for the votes allocated to each regional party list in this region. For each party
#' a party divisor is initialized with 1.
#'
#' Effectively, the objective of the iterative process is to modify the regional divisors
#' and party divisors so that the number of seats in each regional party list equals the
#' number of their votes divided by both the regional and the party divisors.
#'
#' The following two correction steps are executed until this objective is satisfied:
#' \itemize{
#'   \item modify the party divisors such that the apportionment within each
#'     party is correct with the chosen rounding method,
#'   \item modify the regional divisors such that the apportionment within the
#'     region is correct with the chosen rounding method.
#' }
#'
#' @param votes_matrix matrix with votes by party in rows and votes by district in columns.
#' @param seats_cols number of seats per column (districts/regions), predetermined or
#'   calculated with [upper_apportionment()].
#' @param seats_rows number of seats per row (parties/lists), calculated with
#'   [upper_apportionment()].
#' @param method Apportion method that defines how seats are assigned. The
#'   following methods are supported:
#'   \itemize{
#'     \item{`round`: The default Sainte-Laguë/Webster method is the standard
#'           for biproportional apportionment and the only method guaranteed to terminate.}
#'     \item{`wto`: "winner take one" works like "round" with a condition that the party that
#'           got the most votes in a district must get _at least_ one seat ('Majorzbedingung').
#'           The condition does not apply in a district if two or more parties have the same
#'           number of votes and there are not enough seats for these parties. A warning is
#'           issued in this case. Modify the votes matrix to explicitly break ties.}
#'     \item{You can provide a custom function that rounds a matrix (i.e. the
#'           the votes_matrix divided by party and list divisors).}
#'     \item{It is possible to use any divisor method name listed in [proporz()].}
#'   }
#'
#' @returns A seat matrix with district (columns) and party (rows) divisors stored in
#'   attributes.
#'
#' @references Oelbermann, K. F. (2016): Alternate scaling algorithm for biproportional
#'   divisor methods. Mathematical Social Sciences, 80, 25-32.
#'
#' @seealso [biproporz()], [upper_apportionment()]
#'
#' @examples
#' votes_matrix = matrix(c(123,912,312,45,714,255,815,414,215), nrow = 3)
#' district_seats = c(7,5,8)
#' party_seats = c(5,11,4)
#'
#' lower_apportionment(votes_matrix, district_seats, party_seats)
#'
#'
#' # using "winner take one"
#' vm = matrix(c(200,100,10,11), 2,
#'             dimnames = list(c("Party A", "Party B"), c("I", "II")))
#' district_seats = setNames(c(2,1), colnames(vm))
#' ua = upper_apportionment(vm, district_seats)
#'
#' lower_apportionment(vm, ua$district, ua$party, method = "wto")
#'
#' # compare to standard method
#' lower_apportionment(vm, ua$district, ua$party, method = "round")
#'
#' @export
lower_apportionment = function(votes_matrix, seats_cols,
                               seats_rows, method = "round") {
    # check parameters
    M = prep_votes_matrix(votes_matrix, deparse(substitute(votes_matrix)))
    assert(all((seats_cols %% 1) == 0))
    assert(all((seats_rows %% 1) == 0))
    assert(sum(seats_cols) == sum(seats_rows))
    assert(length(seats_cols) == ncol(M) && length(seats_rows) == nrow(M))

    # rounding function from method
    if(is.function(method)) {
        round_func = method
    } else if(method == "round") {
        round_func = function(x) ceil_at(x, 0.5)
    } else if(method == "wto") {
        round_func = create_wto_round_function(votes_matrix, seats_cols, seats_rows)
    } else {
        method_impl <- get_method_implementation(method)
        if(method_impl != "divisor_round") {
            warning('Lower apportionment is only guaranteed to terminate with the default ',
                    'Sainte-Lagu\u00EB/Webster method (method = "round")', call. = FALSE)
        }
        round_func = get_round_function(method_impl)
    }

    # alternate scaling algorithm to find divisors
    divisors = find_matrix_divisors(votes_matrix, seats_cols, seats_rows, round_func)

    # prettier divisors
    divisors <- prettier_divisors(votes_matrix, divisors, round_func)

    # create output
    dD = divisors[["cols"]]; dP = divisors[["rows"]]
    output = round_func(divide_votes_matrix(M, dD, dP))
    dimnames(output) <- dimnames(M)
    attributes(output)$divisors <- list(districts = dD, parties = dP)
    names(attributes(output)$divisors$districts) <- colnames(M)
    names(attributes(output)$divisors$parties) <- rownames(M)
    return(output)
}

#' Calculate raw seat matrix
#'
#' Apply row and column divisors to matrix to get non-rounded seat values.
#'
#' @param M matrix
#' @param col_divisors divisors to apply to columns
#' @param row_divisors divisors to apply to rows
#'
#' @returns matrix with the same dimension as `M` containing non-rounded seat values
#' @keywords internal
divide_votes_matrix = function(M, col_divisors, row_divisors) {
    M_district = matrix(rep(col_divisors, nrow(M)), byrow = TRUE, nrow = nrow(M))
    M_party = matrix(rep(row_divisors, ncol(M)), byrow = FALSE, nrow = nrow(M))

    x = M/M_district/M_party
    x[is.nan(x)] <- 0
    return(x)
}

#' Find divisors for a matrix with alternate scaling
#'
#' @param M votes_matrix
#' @param seats_cols target seats for each column
#' @param seats_rows target seats for each row
#' @param round_func rounding function. Called like
#'   `round_func(M/row_divisors/col_divisors)`, divisors are applied row/col-wise with
#'   [divide_votes_matrix()].
#'
#' @returns list of divisors (column and row)
#' @keywords internal
find_matrix_divisors = function(M, seats_cols, seats_rows, round_func) {
    assert(is.matrix(M))
    assert(is.matrix(round_func(M)))

    # divisor parties
    dR = rep(1, nrow(M))
    dR.min = rep(0.5, nrow(M))
    dR.max = rep(1.5, nrow(M))

    # divisor districts
    dC = round(colSums(M)/seats_cols)
    dC[is.nan(dC)] <- 0
    dC.min = floor(colSums(M)/(seats_cols+1) / max(dR.max))
    dC.max = ceiling(colSums(M)/(seats_cols-1) / min(dR.min))

    # handle districts with only one seat (otherwise leads to infinite dC.max)
    dC.max[seats_cols == 1] <- (colSums(M)+1)[seats_cols == 1]

    # convenience functions to round and summarise
    m. = divide_votes_matrix
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

    # usually less than 20 iterations are needed
    max_iter = getOption("proporz_max_iterations", 1000)
    for(i in seq_len(max_iter)) {
        if(all(c(mc(M,dC,dR) == seats_cols, mr(M,dC,dR) == seats_rows))) {
            return(list(cols = dC, rows = dR))
        }
        # change party divisors
        row_decr = which.min0(mr(M,dC,dR) - seats_rows)
        if(length(row_decr) == 1) {
            dR[row_decr] <- find_divisor(
                M[row_decr,,drop=F]/dC,
                dR[row_decr], dR.min[row_decr],
                seats_rows[row_decr], round_func)
        }

        row_incr = which.max0(mr(M,dC,dR) - seats_rows)
        if(length(row_incr) == 1) {
            dR[row_incr] <- find_divisor(
                M[row_incr,,drop=F]/dC,
                dR[row_incr], dR.max[row_incr],
                seats_rows[row_incr], round_func)
        }

        # change district divisors
        col_decr = which.min0(mc(M,dC,dR) - seats_cols)
        if(length(col_decr) == 1) {
            dC[col_decr] <- find_divisor(
                M[,col_decr,drop=F]/dR,
                dC[col_decr], dC.min[col_decr],
                seats_cols[col_decr], round_func)
        }

        col_incr = which.max0(mc(M,dC,dR) - seats_cols)
        if(length(col_incr) == 1) {
            dC[col_incr] <- find_divisor(
                M[,col_incr,drop=F]/dR,
                dC[col_incr], dC.max[col_incr],
                seats_cols[col_incr], round_func)
        }
    }
    stop("Result is undefined, exceeded maximum number of iterations (", max_iter, ")",
         call. = FALSE)
}

#' Find divisor to assign seats
#'
#' Find a divisor between `divisor_from` and `divisor_to` such as
#' `sum(round_func(votes/divisor))` equals `target_seats`
#'
#' @param votes votes (matrix with only one column or vector, allows to use row/colnames
#'   within `round_func`)
#' @param divisor_from lower bound for divisor search range (is decreased if necessary)
#' @param divisor_to upper bound for divisor search range (is increased if necessary)
#' @param target_seats number of seats to distribute (single number)
#' @param round_func rounding function
#'
#' @return divisor
#' @keywords internal
find_divisor = function(votes,
                        divisor_from, divisor_to,
                        target_seats, round_func) {
    assert(is.matrix(votes))
    assert(any(dim(votes) == 1))
    assert(length(target_seats) == 1)

    # use matrix instead of vector for rownames
    fun = function(divisor) {
        target_seats - sum(round_func(votes/divisor))
    }

    divisor_range = sort(c(divisor_from, divisor_to))

    if(any(is.infinite(votes)) || any(is.nan(votes))) {
        stop("Result is undefined, cannot assign all seats in lower apportionment", call. = FALSE)
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

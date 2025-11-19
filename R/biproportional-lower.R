#' Lower apportionment
#'
#' In the second biproportional apportionment step, party and district divisors are calculated
#' such that the row and column sums of the resulting seats matrix satisfy the constraints
#' given by the upper apportionment.
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
#'     \item{`wto`: "winner take one" works like `round` with a condition that the party that
#'           got the most votes in a district must get _at least_ one seat ('Majorzbedingung',
#'           also called 'strongest party constrained' rule (SPC)). `votes_matrix` must have
#'           row and column names to use this method.
#'           A district winner can only get a seat if they are entitled to one from the upper
#'           apportionment (`seats_rows`).
#'           The condition does not apply in a district if two or more parties have the same
#'           number of votes and there are not enough seats for these parties. A warning is
#'           issued in this case. Modify the votes matrix to explicitly break ties.}
#'     \item{You can provide a custom function that rounds a matrix (i.e. the
#'           the `votes_matrix` divided by party and district divisors) without further
#'           parameters.}
#'     \item{It is possible to use any divisor method name listed in [proporz()].}
#'   }
#'
#' @returns A seat matrix with district (columns) and party (rows) divisors stored in
#'   attributes.
#'
#' @note If the maximum number of optimization iterations is reached, an error is thrown since
#'   no solution can be found. You can overwrite the default (1000) with
#'   `options(proporz_max_iterations = ...)` but it is very likely that the result is undefined
#'   given the structure of the input parameters.
#'
#' @references Oelbermann, K. F. (2016): Alternate scaling algorithm for biproportional
#'   divisor methods. Mathematical Social Sciences, 80, 25-32.
#'
#' @seealso [biproporz()], [upper_apportionment()], [district_winner_matrix()]
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
    assert(length(seats_cols) == ncol(M))
    assert(length(seats_rows) == nrow(M))
    seats_cols <- prep_seats_cols(seats_cols, votes_matrix)
    seats_rows <- prep_seats_rows(seats_rows, votes_matrix)

    # rounding function from method
    if(is.function(method)) {
        round_func = method
    } else if(method == "round") {
        round_func = function(x) ceil_at(x, 0.5)
    } else if(method == "wto") {
        round_func = create_wto_round_function(votes_matrix, seats_cols, seats_rows)
    } else {
        method_impl = get_method_implementation(method)
        round_func = get_round_function(method_impl)
    }

    # alternate scaling algorithm to find divisors
    divisors = find_matrix_divisors(votes_matrix, seats_cols, seats_rows, round_func)

    # prettier divisors
    divisors <- prettier_divisors(votes_matrix, divisors, round_func)

    # create output
    dD = divisors[["cols"]]; dP = divisors[["rows"]]
    output = round_func(divide_votes_matrix(M, dD, dP))
    mode(output) <- "integer"
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
    M_district = row_as_matrix(col_divisors, M)
    M_party = col_as_matrix(row_divisors, M)

    d = M/M_district/M_party
    d[is.nan(d) | is.infinite(d) | is.na(d)] <- 0
    return(d)
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
    check_flow_criterion(M, seats_cols, seats_rows)

    # convenience functions to round and summarise
    m. = divide_votes_matrix
    mc = function(.M,.d,.p) colSums(round_func(m.(.M,.d,.p)))
    mr = function(.M,.d,.p) rowSums(round_func(m.(.M,.d,.p)))

    # divisor parties
    dR = rep(1, nrow(M))
    dR.min = rep(0.5, nrow(M))
    dR.max = rep(1.5, nrow(M))

    # divisor districts
    .colsums = unname(colSums(M))
    dC = .colsums/seats_cols
    dC.min = .colsums / (seats_cols+1) / max(dR.max)
    dC.max = .colsums / pmax(1, seats_cols-1) / min(dR.min)

    which.min0 = function(x) {
        x[x == 0] <- max(x)
        if(length(unique(x)) == 1L) return(NA)
        which.min(x)
    }
    which.max0 = function(x) {
        x[x == 0] <- min(x)
        if(length(unique(x)) == 1L) return(NA)
        which.max(x)
    }

    # usually less than 20 iterations are needed
    max_iter = getOption("proporz_max_iterations", 1000)
    target_diff_prev = sum(2*seats_cols)
    for(i in seq_len(max_iter)) {
        # break conditions
        if(any(round_func(m.(M,dC,dR)) %% 1 != 0)) {
            stop("Rounding function does not return integers", call. = FALSE)
        }
        target_diff = sum(abs(mc(M,dC,dR) - seats_cols)) + sum(abs(mr(M,dC,dR) - seats_rows))
        if(target_diff > target_diff_prev) {
            stop("Result is undefined, cannot assign all seats in lower apportionment", call. = FALSE)
        }
        target_diff_prev <- target_diff
        if(sum(target_diff) == 0) {
            return(list(cols = dC, rows = dR))
        }

        # change party divisors
        row_decr = which.min0(mr(M,dC,dR) - seats_rows)
        if(!is.na(row_decr)) {
            dR[row_decr] <- find_divisor(
                div0(M[row_decr,,drop=FALSE], dC),
                dR[row_decr], dR.min[row_decr],
                seats_rows[row_decr], round_func)
        }

        row_incr = which.max0(mr(M,dC,dR) - seats_rows)
        if(!is.na(row_incr)) {
            dR[row_incr] <- find_divisor(
                div0(M[row_incr,,drop=FALSE], dC),
                dR[row_incr], dR.max[row_incr],
                seats_rows[row_incr], round_func)
        }

        # change district divisors
        col_decr = which.min0(mc(M,dC,dR) - seats_cols)
        if(!is.na(col_decr)) {
            dC[col_decr] <- find_divisor(
                div0(M[,col_decr,drop=FALSE], dR),
                dC[col_decr], dC.min[col_decr],
                seats_cols[col_decr], round_func)
        }

        col_incr = which.max0(mc(M,dC,dR) - seats_cols)
        if(!is.na(col_incr)) {
            dC[col_incr] <- find_divisor(
                div0(M[,col_incr,drop=FALSE], dR),
                dC[col_incr], dC.max[col_incr],
                seats_cols[col_incr], round_func)
        }

        # no rows/cols to change found
        if(is.na(row_decr) && is.na(row_incr) && is.na(col_decr) && is.na(col_incr)) {
            # ensure that there's at least one seat distributed initially
            if(sum(mc(M,dC,dR)) == 0) {
                for(s in seq(0.5,max(seats_cols),0.5)) {
                    dC <- .colsums/(seats_cols+s)
                    if(sum(mc(M,dC,dR)) > 0) break
                }
            } else {
                stop("Result is undefined, tied votes and multiple possible seat assignments",
                     call. = FALSE)
            }
        }
    }
    stop("Result is undefined, exceeded maximum number of iterations (", max_iter, ")",
         call. = FALSE)
}

#' Find divisor to assign seats
#'
#' Find a divisor between `divisor_from` and `divisor_to` such that
#' `sum(round_func(votes/divisor))` equals `target_seats`
#'
#' @param votes votes (matrix with only one column or vector, allows to use row/colnames
#'   within `round_func`)
#' @param divisor_from lower bound for divisor search range (is decreased if necessary)
#' @param divisor_to upper bound for divisor search range (is increased if necessary)
#' @param target_seats number of seats to distribute (single number)
#' @param round_func rounding function
#'
#' @returns divisor
#' @keywords internal
find_divisor = function(votes,
                        divisor_from, divisor_to,
                        target_seats, round_func) {
    assert(is.matrix(votes))
    assert(any(dim(votes) == 1))
    assert(length(target_seats) == 1)
    assert(all(!is.infinite(votes)) && all(!is.na(votes)))
    assert(!is.na(divisor_from) && !is.na(divisor_to))
    assert(divisor_from > 0)

    # use matrix instead of vector for rownames
    fun = function(divisor) {
        target_seats - sum(round_func(div0(votes, divisor)))
    }

    divisor_range = sort(c(divisor_from, divisor_to))

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

bisect = function(f, x1, x2, tol = 1e-9, max_iterations = 1000) {
    assert(length(x1) == 1 && length(x2) == 1 && length(tol) == 1)
    assert((f(x1) <= 0 && f(x2) >= 0) || (f(x1) >= 0 && f(x2) <= 0))
    assert(x1 >= 0 && x2 >= 0 && x1 < x2)
    assert(!is.infinite(x1) && !is.infinite(x2))
    assert(!is.nan(x1) && !is.nan(x2))

    for(i in seq_len(max_iterations)) {
        x <- (x1 + x2) / 2
        if(f(x) == 0 || (x2-x1) < tol) {
            return(x)
        }
        if(sign(f(x)) == sign(f(x1))) {
            x1 <- x
        } else {
            x2 <- x
        }
    }
    stop("Exceeded maximum number of bisection iterations (", max_iterations, ")") # nocov
}

prep_seats_cols = function(seats_cols, votes_matrix) {
    if(!identical(sort(colnames(votes_matrix)), sort(names(seats_cols)))) {
        stop("seats_cols must have the same names as the votes_matrix column", call. = FALSE)
    }
    if(!is.null(names(seats_cols))) {
        seats_cols <- seats_cols[colnames(votes_matrix)]
    }
    return(seats_cols)
}

prep_seats_rows = function(seats_rows, votes_matrix) {
    if(!identical(sort(rownames(votes_matrix)), sort(names(seats_rows)))) {
        stop("seats_rows must have the same names as the votes_matrix rows", call. = FALSE)
    }
    if(!is.null(names(seats_rows))) {
        seats_rows <- seats_rows[rownames(votes_matrix)]
    }
    return(seats_rows)
}

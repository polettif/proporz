# If last party with seat and first without a seat have the same matrix_quotient
# from highest_averages_method the result is undefined as both parties could get the last
# seat
check_edge_quotient = function(mtrx_quotient, n_seats) {
    ordered_quotients = order(mtrx_quotient, decreasing = TRUE)
    quotient_last_with = mtrx_quotient[ordered_quotients[n_seats]]
    quotient_first_without = mtrx_quotient[ordered_quotients[n_seats+1]]

    if(is.nan(quotient_first_without)) quotient_first_without <- 0

    if(quotient_last_with == quotient_first_without) {
        stop("Result is undefined")
    }
}

check_equal_entries = function(vec) {
    stopifnot(is.vector(vec))
    if(length(unique(vec)) != length(vec)) {
        eq_index = which(vec == names(which.max(table(vec))))
        eq_str = paste0(eq_index, collapse = " & ")
        stop("Result is undefined: Equal remainder for two parties (position ", eq_str ,")",
             call. = FALSE)
    }
}

quorum_votes = function(votes, quorum) {
    check_votes(votes)
    stopifnot(length(quorum) == 1, is.numeric(quorum), quorum >= 0)

    if(quorum < 1) {
        quorum = ceiling(sum(votes)*quorum)
    }
    votes[which(votes < quorum)] <- 0
    return(votes)
}

bisect = function(f, x1, x2, tol = 1e-9) {
    stopifnot(length(x1) == 1, length(x2) == 1, length(tol) == 1, x1 < x2)
    stopifnot((f(x1) <= 0 & f(x2) >= 0) | (f(x1) >= 0 & f(x2) <= 0))

    for(i in 1:1e9) {
        x <- (x1 + x2)/2
        if(f(x) == 0 || (x2-x1) < tol) {
            return(x)
        }
        if(sign(f(x)) == sign(f(x1))) {
            x1 <- x
        } else {
            x2 <- x
        }
    }
    stop("Exceeded maximum number of iterations")
}

#' Pivot long data.frame to wide matrix
#'
#' @param df data.frame in long format with exactly 3 columns
#'
#' @note This function exists because I wanted to have no dependencies.
#'       Wrangling with reshape isn't fun at all and it became glaringly
#'       apparent why better tools like tidyr exist
#' @export
pivot_to_matrix = function(df) {
    stopifnot(ncol(df) == 3)
    stopifnot(nrow(df) == nrow(unique(df[1:2])))
    tbl = table(df)
    stopifnot(max(tbl) == 1)
    apply(tbl, c(1,2), function(x) sum(as.numeric(names(x))*unname(x)))
}

#' Pivot wide matrix to long data.frame
#'
#' @param matrix_wide matrix in wide format
#' @param value_colname name for the data.frame new column
#'
#' @export
pivot_to_df = function(matrix_wide, value_colname = "values") {
    if(is.null(dimnames(matrix_wide))) {
        colnames(matrix_wide) <- 1:ncol(matrix_wide)
        rownames(matrix_wide) <- 1:nrow(matrix_wide)
        names(dimnames(matrix_wide)) <- c("row", "col")
    }

    # create empty data frame with values
    ids_keys = dimnames(matrix_wide)
    new_df = data.frame(
        .id = rep(ids_keys[[1]], each = length(ids_keys[[2]])),
        .key = rep(ids_keys[[2]], length(ids_keys[[1]])),
        stringsAsFactors = FALSE
    )
    colnames(new_df) <- names(dimnames(matrix_wide))

    # "byrow" indices for matrix
    values_indices = c(vapply(1:nrow(matrix_wide),
                              function(i) seq(i, length(matrix_wide), nrow(matrix_wide)),
                              1:ncol(matrix_wide)))

    # select values by index
    new_df[[value_colname]] <- matrix_wide[values_indices]
    return(new_df)
}

check_n_seats = function(n_seats) {
    if(length(n_seats) == 1 && !is.null(n_seats) && !is.na(n_seats) && n_seats >= 0) {
        return()
    }
    stop("n_seats must be one number >= 0", call. = FALSE)
}

check_votes = function(votes) {
    if(is.numeric(votes) && all(!is.na(votes)) && all(votes >= 0)) {
        return()
    }
    stop("votes must be numeric >= 0", call. = FALSE)
}

.sample_votes = function(n_non_zero, n_zero) {
    repeat {
        x = round(stats::runif(n_non_zero, 10, 1000))
        if(length(unique(x)) == n_non_zero) {
            break
        }
    }
    return(c(x, rep(0, n_zero)))
}

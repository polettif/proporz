#' Get district and party divisors from biproporz result
#'
#' Show the district and party divisors used to assign seats.
#' This method provides easier access to divisors stored in
#' `attributes(...)$divisors`.
#'
#' @param biproporz_result a matrix created by [biproporz()] or a
#'   data.frame created by [pukelsheim()]
#'
#' @returns The district and party divisors (named "districts" and "parties") in a list, each
#'   as a vector
#'
#' @examples
#' seats_matrix = biproporz(uri2020$votes_matrix, uri2020$seats_vector)
#' get_divisors(seats_matrix)
#'
#' seats_df = pukelsheim(pivot_to_df(uri2020$votes_matrix),
#'                       data.frame(names(uri2020$seats_vector), uri2020$seats_vector))
#' get_divisors(seats_df)
#'
#' # summary() also prints the divisors for a biproporz matrix
#' summary(seats_matrix)
#'
#' @export
get_divisors = function(biproporz_result) {
    attributes(biproporz_result)[["divisors"]]
}

prettier_divisors = function(votes_matrix, divisors, round_func) {
    assert(all(names(divisors) == c("cols", "rows")))
    dD <- divisors[["cols"]]
    dP <- divisors[["rows"]]

    dP <- .round_matrix_divisors(dP, function(x) round_func(divide_votes_matrix(votes_matrix, dD, x)))
    dD <- .round_matrix_divisors(dD, function(x) round_func(divide_votes_matrix(votes_matrix, x, dP)))

    return(list(cols = dD, rows = dP))
}

# round divisors to as few digits as possible
.round_matrix_divisors = function(divisors, round_matrix_func) {
    expected = round_matrix_func(divisors)

    # start with divisors with the most digits
    for(i in order(n_digits(divisors), decreasing = TRUE)) {
        # see if rounded down or up to k digits leads to the same result
        for(k in seq(0,15)) {
            divisors_cand = divisors

            # floor
            divisors_cand[i] <- .floor_k(divisors[i], k)
            if(identical(round_matrix_func(divisors_cand), expected)) {
                divisors <- divisors_cand
                break
            }

            # ceil
            divisors_cand[i] <- .ceiling_k(divisors[i], k)
            if(identical(round_matrix_func(divisors_cand), expected)) {
                divisors <- divisors_cand
                break
            }
        }
    }

    return(divisors)
}

n_digits = function(vec) {
    digits = rep(NA, length(vec))
    for(k in seq(0, 15)) {
        x1 = (vec*10^k)
        x2 = floor(x1)
        digits[(x1-x2) < 1e-8 & is.na(digits)] <- k
        if(all(!is.na(digits))) break
    }
    return(digits)
}

.floor_k = function(x, k) {
    x <- round(x, k+15)
    floor(x*10^k)/10^k
}
.ceiling_k = function(x, k) {
    x <- round(x, k+15)
    ceiling(x*10^k)/10^k
}

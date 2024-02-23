#' @export
print.proporz_matrix = function(x, ...) {
    y <- as.matrix(x)
    print(y)
    invisible(x)
}

#' @export
as.matrix.proporz_matrix = function(x, ...) {
    matrix(x, nrow = nrow(x), dimnames = dimnames(x))
}

#' Get district and party divisors from biproporz result
#'
#' Show the district and party divisors used to assign seats.
#' This method provides easier access to divisors stored in
#' `attributes(...)$divisors`
#'
#' @param biproporz_result a matrix created by [biproporz()]
#'                         or a data.frame created by [pukelsheim()]
#'
#' @returns The district and party divisors in a list, each as a vector
#'
#' @examples
#' seats_matrix = biproporz(uri2020$votes_matrix, uri2020$seats_vector)
#' get_divisors(seats_matrix)
#'
#' @export
get_divisors = function(biproporz_result) {
    attributes(biproporz_result)$divisors
}

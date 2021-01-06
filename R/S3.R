print.proporz_matrix = function(x, ...) {
    y <- as.matrix(x)
    print(y)
    invisible(x)
}

as.matrix.proporz_matrix = function(x, ...) {
	matrix(x, nrow = nrow(x), dimnames = dimnames(x))
}

#' Get district and party divisors
#'
#' Show the district and party divisors used for to assign seats. This method
#' provides easier access to divisors stored in
#' attributes(...)$divisors
#'
#' @param biproportional_result a matrix created by \code{\link{biproportional}}
#'                              or a data.frame created by \code{\link{pukelsheim}}
#'
#' @examples
#' votes_matrix = matrix(c(51,60,63,98,100,102,45,120,144), nrow = 3)
#' district_seats = 4:6
#'
#' seats_matrix = biproportional(votes_matrix, district_seats)
#' divisors(seats_matrix)
#' #> $districts
#' #> [1] 40.90625 67.06318 52.00000
#' #>
#' #> $parties
#' #> [1] 0.9694748 0.9921875 1.0234375
#'
#' @export
divisors = function(biproportional_result) {
    attributes(biproportional_result)$divisors
}

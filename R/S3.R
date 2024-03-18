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


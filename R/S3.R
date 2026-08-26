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

#' @export
#' @importFrom stats addmargins
summary.proporz_matrix = function(object, ...) {
    divisors = get_divisors(object)
    divisors_rows = divisors[["parties"]]
    divisors_cols = divisors[["districts"]]

    if(!identical(rownames(object), names(divisors_rows)) ||
       !identical(colnames(object), names(divisors_cols))) {
        # t()-transformed matrix?
        if(identical(colnames(object), names(divisors_rows)) &&
           identical(rownames(object), names(divisors_cols))) {
            divisors_rows <- divisors[["districts"]]
            divisors_cols <- divisors[["parties"]]
        } else {
            stop("proporz_matrix must have dimnames identical to divisor names", call. = FALSE)
        }
    }

    summary_mtx = addmargins(object)
    colnames(summary_mtx)[ncol(summary_mtx)] <- "(sum)"
    rownames(summary_mtx)[nrow(summary_mtx)] <- "(sum)"
    mode(summary_mtx) <- "character"

    summary_mtx <- cbind(summary_mtx, `(divisor)` = c(divisors_rows, ""))
    summary_mtx <- rbind(summary_mtx, `(divisor)` = c(divisors_cols, "", ""))

    summary_tbl = cbind(X =rownames(summary_mtx), as.data.frame(summary_mtx))
    colnames(summary_tbl)[1] <- ""
    class(summary_tbl) <- c("proporz_matrix_summary", "data.frame")

    return(summary_tbl)
}

#' @export
print.proporz_matrix_summary = function(x, ...) {
    print.data.frame(x, row.names = FALSE, right = TRUE)
    invisible(x)
}

.as_tibble = function(df) {
    tibble_attr = c("class", "row.names", "names")
    attributes(df) <- attributes(df)[c(tibble_attr, setdiff(names(attributes(df)), tibble_attr))]

    attr(df, "class") <- c("tbl_df", "tbl", "data.frame")
    attr(df, "row.names") <- c(NA, -nrow(df))

    return(df)
}

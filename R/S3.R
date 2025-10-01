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

    summary_tbl = as.data.frame(as.matrix(object))
    rownames(summary_tbl) <- NULL
    summary_tbl <- cbind(X = rownames(object), summary_tbl)
    summary_tbl[["(sum)"]] <- rowSums(object)
    summary_tbl[["(divisor)"]] <- divisors_rows

    district_sums_row = cbind(
        X = "(sum)",
        as.data.frame(as.list(colSums(object)), check.names = FALSE),
        "(sum)" = sum(object), "(divisor)" = NA)

    district_divisors_row = cbind(
        X = "(divisor)",
        as.data.frame(as.list(divisors_cols), check.names = FALSE),
        "(sum)" = NA, "(divisor)" = NA)

    summary_tbl <- rbind(summary_tbl, district_sums_row)
    summary_tbl <- rbind(summary_tbl, district_divisors_row)

    for(j in seq_len(ncol(summary_tbl))) {
        summary_tbl[[j]] <- as.character(summary_tbl[[j]])
        summary_tbl[[j]][is.na(summary_tbl[[j]])] <- ""
    }
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

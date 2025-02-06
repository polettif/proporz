#' Pivot long data.frame to wide matrix and vice versa
#'
#' Create a matrix in 'wide' format from a data.frame with 3 columns with
#' `pivot_to_matrix` or create a data.frame in long format from a matrix with
#' `pivot_to_df.`
#'
#' These pivot functions are used to prepare data for [biproporz()] in
#' [pukelsheim()]. They are not supposed to cover general use cases or provide
#' customization. They mainly exist because reshape is hard to handle and the
#' package should have no dependencies.
#
#' @param df_long data.frame in long format with exactly 3 columns
#' @param matrix_wide matrix in wide format
#' @param value_colname name for the new value column in the
#'                      resulting data.frame
#'
#' @returns A data.frame with 3 columns or a matrix. Note that the results are
#'          sorted by the first and second column (data.frame) or row/column
#'          names (matrix).
#'
#' @examples
#' # From data.frame to matrix
#' df = data.frame(party = c("A", "A", "A", "B", "B", "B"),
#'                 region = c("III", "II", "I", "I", "II", "III"),
#'                 seats = c(5L, 3L, 1L, 2L, 4L, 6L))
#' pivot_to_matrix(df)
#'
#' # from matrix to data.frame
#' mtrx = matrix(1:6, nrow = 2)
#' pivot_to_df(mtrx)
#'
#' # from matrix to data.frame using dimnames
#' dimnames(mtrx) <- list(party = c("A", "B"), region = c("I", "II", "III"))
#' pivot_to_df(mtrx, "seats")
#'
#' # Note that pivot results are sorted
#' pivot_to_df(pivot_to_matrix(df)) == df[order(df[[1]], df[[2]]),]
#'
#' @export
pivot_to_matrix = function(df_long) {
    assert(ncol(df_long) == 3)
    assert(nrow(df_long) == nrow(unique(df_long[1:2])))

    asnum = as.numeric
    if(is.integer(df_long[[3]])) asnum = as.integer

    tbl = table(df_long)
    assert(max(tbl) == 1)
    apply(tbl, c(1,2), function(x) sum(asnum(names(x))*unname(x)))
}

#' @rdname pivot_to_matrix
#' @export
pivot_to_df = function(matrix_wide, value_colname = "values") {
    if(is.null(colnames(matrix_wide))) colnames(matrix_wide) <- seq_len(ncol(matrix_wide))
    if(is.null(rownames(matrix_wide))) rownames(matrix_wide) <- seq_len(nrow(matrix_wide))
    if(is.null(names(dimnames(matrix_wide)))) {
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
    values_indices = c(vapply(seq_len(nrow(matrix_wide)),
                              function(i) seq(i, length(matrix_wide), nrow(matrix_wide)),
                              seq_len(ncol(matrix_wide))))

    # select values by index
    new_df[[value_colname]] <- matrix_wide[values_indices]
    return(new_df)
}

col_as_matrix = function(x, mtrx) {
    assert(length(x) == nrow(mtrx))
    matrix(rep(x, ncol(mtrx)), nrow = nrow(mtrx))
}

row_as_matrix = function(x, mtrx) {
    assert(length(x) == ncol(mtrx))
    matrix(rep(x, each = nrow(mtrx)), nrow = nrow(mtrx))
}

row_maxs = function(mtrx) {
    apply(mtrx, 1, max)
}

col_maxs = function(mtrx) {
    apply(mtrx, 2, max)
}

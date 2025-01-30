check_params.pukelsheim = function(votes_df, district_seats_df, new_seats_col,
                                   use_list_votes, winner_take_one,
                                   .votes_df, .district_seats_df) {
    assert(is.character(new_seats_col) && length(new_seats_col) == 1)
    assert(is.logical(use_list_votes) && !is.na(use_list_votes) && length(use_list_votes) == 1)
    assert(is.logical(winner_take_one) && !is.na(winner_take_one) && length(winner_take_one) == 1)

    if(!is.data.frame(votes_df) || ncol(votes_df) != 3) {
        stop("`", .votes_df, "` must be a data frame with 3 columns in the ",
             "following order:\nparty, district and votes (names can differ).",
             call. = FALSE)
    }

    if(!is.numeric(votes_df[[3]]) || any(votes_df[[3]] < 0)) {
        stop("Vote values in `", .votes_df,
             "`s third column must be numbers >= 0.", call. = FALSE)
    }

    if(!is.data.frame(district_seats_df)) {
        stop("`", .district_seats_df, "` must be a data.frame.", call. = FALSE)
    }
    if(length(unique(district_seats_df[[1]])) != nrow(district_seats_df)) {
        stop("District ids in `", .district_seats_df,
             "` are not unique.", call. = FALSE)
    }
    if(nrow(votes_df[,c(1,2)]) != nrow(unique(votes_df[,c(1,2)]))) {
        stop("There are duplicate party-district pairs in `", .votes_df, "`.",
             call. = FALSE)
    }

    if(!all(district_seats_df[[1]] %in% votes_df[[2]])) {
        if(all(district_seats_df[[1]] %in% votes_df[[1]])) {
            stop("District ids not found in second column of `", .votes_df,
                 "`. Are columns in the correct order (party, district, votes)?",
                 call. = FALSE)
        }
        stop("Not all district ids in `", .district_seats_df, "`s first column ",
             "exist in `", .votes_df, "`s second column.", call. = FALSE)
    }

    if(!all(votes_df[[2]] %in% district_seats_df[[1]])) {
        stop("Not all district ids in `", .votes_df, "`s second column exist in `",
             .district_seats_df, "`s first column.", call. = FALSE)
    }
}

prep_votes_matrix = function(votes_matrix, votes_matrix.name) {
    vmn = paste0("`", votes_matrix.name, "`")
    if(!is.matrix(votes_matrix)) {
        stop(vmn, " must be a matrix.", call. = FALSE)
    }
    if(any(is.na(votes_matrix)) || any(votes_matrix < 0) || !is.numeric(votes_matrix)) {
        stop("Votes in ", vmn, " must be numbers >= 0.", call. = FALSE)
    }
    if(!is.null(rownames(votes_matrix)) &&
       length(unique(rownames(votes_matrix))) != nrow(votes_matrix)) {
        stop("rownames in ", vmn , " must be unique.", call. = FALSE)
    }
    if(!is.null(colnames(votes_matrix)) &&
       length(unique(colnames(votes_matrix))) != ncol(votes_matrix)) {
        stop("colnames in ", vmn, " must be unique.", call. = FALSE)
    }

    return(votes_matrix)
}

prep_method = function(method) {
    if(!length(method) %in% c(1,2)) {
        stop("Only one or two methods allowed.", call. = FALSE)
    }
    if(length(method) == 1) {
        if(method == "wto") {
            method = list("round", "wto")
        } else {
            method <- list(method, method)
        }
    }
    if(any(method == "largest_remainder_method")) {
        stop('Cannot use "largest_remainder_method", only divisor methods ',
             'are possible in biproportional apportionment.', call. = FALSE)
    }

    return(method)
}

prep_district_seats = function(district_seats, votes_matrix,
                               .district_seats.name, .votes_matrix.name) {
    if(!(is.vector(district_seats, "numeric") || is.data.frame(district_seats))) {
        stop("`", .district_seats.name, "` must be a numeric vector, data.frame or a single number.",
             call. = FALSE)
    }
    if(length(district_seats) > 1) {
        if(is.data.frame(district_seats)) {
            district_seats <- setNames(district_seats[[2]], district_seats[[1]])
        }
        if(ncol(votes_matrix) != length(district_seats)) {
            stop("`", .votes_matrix.name,
                 "` needs to have districts as columns and parties as rows.",
                 call. = FALSE)
        }
        if(!identical(sort(colnames(votes_matrix)), sort(names(district_seats)))) {
            stop("`", .district_seats.name,
                 "` needs to have the same names as the columns in `",
                 .votes_matrix.name, "`.", call. = FALSE)
        }
        if(!is.null(colnames(votes_matrix))) { # seats vector is named/unnamed like matrix
            district_seats <- district_seats[colnames(votes_matrix)]
        }
    }
    if(sum(district_seats %% 1) != 0) {
        stop("`", .district_seats.name, "` must be integers.", call. = FALSE)
    }
    assert(is.atomic(district_seats))

    return(district_seats)
}

# transform data.frame to named vector
prep_district_seats_df = function(district_seats_df) {
    district_seats <- district_seats_df[[2]]
    names(district_seats) <- district_seats_df[[1]]
    return(district_seats)
}

check_enough_biproporz_seats = function(M, seats_cols, seats_rows) {
    # row check
    seats_cols_M = matrix(rep(seats_cols, each = nrow(M)), nrow = nrow(M))
    row_check = rowSums(seats_cols_M * (M > 0))
    not_enough = unname(which(seats_rows > row_check))
    party_names = if(!is.null(rownames(M))) rownames(M) else as.character(seq_len(nrow(M)))
    if(length(not_enough) > 0) {
        stop("Not enough seats for party ", collapse_names(party_names[not_enough]),
             " in all districts with non-zero votes (", sum(seats_rows[not_enough]),
             " seats necessary, available at most: ", sum(row_check[not_enough]), ")",
             call. = FALSE)
    }

    # col check
    seats_row_M = matrix(rep(seats_rows, ncol(M)), ncol = ncol(M))
    col_check = colSums(seats_row_M * (M > 0))
    not_enough = unname(which(seats_cols > col_check))
    district_names = if(!is.null(colnames(M))) colnames(M) else as.character(seq_len(ncol(M)))
    if(length(not_enough) > 0) {
        stop("Not enough seats in district ", collapse_names(district_names[not_enough]),
             " (", sum(seats_cols[not_enough]),
             " seats necessary, available at most: ", sum(col_check[not_enough]), ")",
             call. = FALSE)
    }

    # block check (submatrices in this case don't share any non-zero columns or rows)
    if(length(unique(rowSums(M > 0))) == 1 && length(unique(colSums(M > 0))) == 1) {
        row_block_index = apply((M > 0) * 1, 1, paste, collapse = "")
        col_block_index = apply((M > 0) * 1, 2, paste, collapse = "")

        if(length(unique(row_block_index)) < nrow(M) || length(unique(col_block_index)) < ncol(M)) {
            block_index_M = matrix(paste(rep(row_block_index, ncol(M)), rep(col_block_index, each = nrow(M))), nrow = nrow(M))

            for(bi in unique(block_index_M)) {
                m = M
                m[block_index_M != bi] <- 0
                sc = sum(seats_cols[colSums(m) == 0])
                sr = sum(seats_rows[rowSums(m) == 0])
                if(sc != sr) {
                    stop("Not enough seats available in submatrix [c(",
                         paste(unique(row(m)[m > 0]), collapse = ","),
                         "), c(", paste(unique(col(m)[m > 0]), collapse = ",") ,")]", call. = FALSE)
                }
            }
        }
    }

    invisible(TRUE)
}

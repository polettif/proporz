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
    if(sum(votes_matrix %% 1) != 0) {
        stop(vmn, " must only contain integers.", call. = FALSE)
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
    if(!(is.vector(district_seats) || is.data.frame(district_seats))) {
        stop("`", .district_seats.name, "` must be a vector, data.frame or a single number.",
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
        if(!is.null(colnames(votes_matrix))) {
            if(is.null(names(district_seats)) ||
               !all(sort(colnames(votes_matrix)) == sort(names(district_seats)))) {
                stop(.district_seats.name,
                     " needs to have the same names as the columns in ",
                     .votes_matrix.name, ".", call. = FALSE)
            }
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

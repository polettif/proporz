check_params.pukelsheim = function(votes_df, district_seats_df, new_seats_col, use_list_votes,
                                   .votes_df, .district_seats_df) {
    stopifnot(is.character(new_seats_col))
    stopifnot(is.logical(use_list_votes))
    if(!is.data.frame(votes_df) || ncol(votes_df) != 3) {
        stop(.votes_df, " must be a data frame with 3 columns in the following order:\n",
             "party, district and votes (names can differ)", call. = F)
    }

    if(!is.numeric(votes_df[[3]]) | any(votes_df[[3]] < 0)) {
        stop("Vote values in ",
             .votes_df,
             "'s third column must be numbers >= 0", call. = F)
    }

    if(length(unique(district_seats_df[[1]])) != nrow(district_seats_df)) {
        stop("District ids are not unique", call. = F)
    }
    if(nrow(votes_df[,c(1,2)]) != nrow(unique(votes_df[,c(1,2)]))) {
        stop("There are duplicate party-district pairs in ", .votes_df, call. = F)
    }

    if(!all(district_seats_df[[1]] %in% votes_df[[2]])) {
        if(all(district_seats_df[[1]] %in% votes_df[[1]])) {
            stop("District ids not found in 2nd column. Are ", .votes_df,
                 "'s columns in the correct order (party, district, votes)?", call. = F)
        }
        stop("Not all district ids in ", .district_seats_df, "'s 1st column exist in ",
             .votes_df, "'s 2nd column", call. = F)
    }

    if(!all(votes_df[[2]] %in% district_seats_df[[1]])) {
        stop("Not all district ids in ", .votes_df, "'s 2nd column exist in ",
             .district_seats_df, "'s 1st column", call. = F)
    }
}

prep_votes_matrix = function(votes_matrix, votes_matrix.name) {
    if(!is.matrix(votes_matrix)) {
        stop(votes_matrix.name, " must be a matrix", call. = FALSE)
    }
    if(sum(votes_matrix %% 1) != 0) stop("votes_matrix must only contain integers", call. = F)

    return(votes_matrix)
}

prep_method = function(method) {
    if(!length(method) %in% c(1,2)) {
        stop("Only one or two methods allowed", call. = F)
    }
    if(length(method) == 1) {
        method = c(method, method)
    }
    stopifnot(length(method) == 2)
    if(any(method == "quota_largest_remainder")) {
        stop("Only divisor methods possible", call. = F)
    }

    return(method)
}

prep_district_seats = function(district_seats, votes_matrix,
                               .district_seats.name, .votes_matrix.name) {

    if(length(district_seats) > 1) {
        if(is.data.frame(district_seats)) {
            district_seats <- setNames(district_seats[[2]], district_seats[[1]])
        }
        if(ncol(votes_matrix) != length(district_seats)) {
            stop(.votes_matrix.name,
                 " needs to have districts as columns and parties as rows", call. = FALSE)
        }
        if(!is.null(colnames(votes_matrix))) {
            if(is.null(names(district_seats)) ||
               !all(sort(colnames(votes_matrix)) == sort(names(district_seats)))) {
                stop(.district_seats.name,
                     " needs to have the same names as the columns in ",
                     .votes_matrix.name, call. = FALSE)
            }
            district_seats <- district_seats[colnames(votes_matrix)]
        }
    }
    if(sum(district_seats %% 1) != 0) stop("district_seats must be integers", call. = F)

    return(district_seats)
}

# transform data.frame to named vector
prep_district_seats_df = function(district_seats_df) {
    district_seats <- district_seats_df[[2]]
    names(district_seats) <- district_seats_df[[1]]
    return(district_seats)
}

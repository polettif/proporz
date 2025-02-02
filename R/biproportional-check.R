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

# The flow-criterion is violated if the total number of seats of some set of parties exceeds
# the number of seats that are rewarded to the districts in which these parties campaign.
# -- Oelbermann, K. F. (2016)
check_flow_criterion = function(M, seats_cols, seats_rows) {
    names(seats_rows) <- seq_along(seats_rows)
    parties_with_seats = seats_rows[seats_rows > 0]
    parties_without_seats = seats_rows[seats_rows == 0]

    # skip check for party counts that are too big too handle with expand.grid and a loop
    if(length(parties_with_seats) > 16) {
        return(invisible(TRUE))
    }

    # create party combination sets (only use parties with seats in expand.grid)
    party_comb_set = t(expand.grid(rep(list(c(TRUE, FALSE)), length(parties_with_seats))))
    rownames(party_comb_set) <- names(parties_with_seats)
    party_comb_set <- party_comb_set[,colSums(party_comb_set) > 0,drop=F]

    if(length(parties_without_seats) > 0) {
        party_comb_set_0 = matrix(FALSE, nrow = length(parties_without_seats), ncol = ncol(party_comb_set))
        rownames(party_comb_set_0) <- names(parties_without_seats)
        party_comb_set <- rbind(party_comb_set, party_comb_set_0)
    }
    party_comb_set <- party_comb_set[as.character(seq_along(seats_rows)),,drop=FALSE]
    party_comb_set <- party_comb_set[,order(colSums(party_comb_set)),drop=FALSE]

    for(p in seq_len(ncol(party_comb_set))) {
        i = c(party_comb_set[,p])
        j = colSums((M > 0)[i,,drop=F]) > 0
        party_seats_necessary = sum(seats_rows[i])
        district_seats_available = sum(seats_cols[j])

        if(party_seats_necessary > district_seats_available) {
            stop("Not enough seats for ", num_word("party ", "parties ", i),
                 collapse_names(i, rownames(M)),
                 " in ", num_word("district ", "districts ", j),
                 collapse_names(j, colnames(M)),
                 "\n(", party_seats_necessary, " seats necessary",
                 ", ", district_seats_available, " available)",
                 call. = FALSE)
        }
    }

    invisible(TRUE)
}

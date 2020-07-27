#' Biproportional apportionment with data frames
#'
#' Parties failing to reach at least one quorum have their votes set to zero.
#'
#' @param votes_df data.frame (long format) with 3 columns: party id/name,
#'                 district id/name, and votes (actual colnames can differ)
#' @param district_seats_df data.frame with 2 colums: district id/name,
#'                          number of seats for a district (actual colnames
#'                          can differ)
#' @param new_seats_col name of the new column
#' @inheritParams biprop_quorum
#'
#' @examples
#' votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
#' district_seats_df = unique(zug2018[c("entity_id", "election_mandates")])
#'
#' seats_df = pukelsheim(votes_df,
#'                       district_seats_df,
#'                       quorum_districts = 0.05,
#'                       quorum_total = 0.03)
#'
#' head(seats_df)
#' #>   list_id entity_id list_votes seats
#' #> 1       2      1701       8108     2
#' #> 2       1      1701       2993     1
#' #> 3       3      1701      19389     3
#' #> 4       4      1701      14814     2
#' #> 5       5      1701       4486     1
#' #> 6       6      1701      15695     3
#'
#' @export
pukelsheim = function(votes_df, district_seats_df,
                      new_seats_col = "seats",
                      quorum_districts = 0,
                      quorum_total = 0) {
    stopifnot(is.numeric(quorum_districts), is.numeric(quorum_total))
    stopifnot(is.character(new_seats_col))
    if(!is.data.frame(votes_df) || ncol(votes_df) != 3) {
        stop(deparse(substitute(votes_df)),
             " must be a data frame with 3 columns in the following
             order: party, district and votes (names can differ)")
    }
    if(length(unique(votes_df[,2])) != nrow(district_seats_df)) {
        stop("Not all districts have a number of seats assigned or
             there are more assignments than districts")
    }
    if(!all(district_seats_df[[1]] %in% votes_df[[2]])) {
        if(all(district_seats_df[[1]] %in% votes_df[[1]])) {
            stop("District ids not found in 2nd column. Are ",
                 deparse(substitute(votes_df)),
                 "'s columns in the correct order (party, district, votes)?")
        }
        stop("Not all district ids in ",
             deparse(substitute(district_seats_df)),
             "'s 1st column exist in ",
             deparse(substitute(votes_df)),
             "'s 2nd column")
    }
    if(!is.numeric(votes_df[[3]]) | any(votes_df[[3]] < 0)) {
        stop("Vote values in ",
             deparse(substitute(votes_df)),
             "'s third column must be numbers >= 0")
    }

    # Create votes matrix
    votes_matrix = pivot_to_matrix(votes_df) # list_ids must be rows

    # "deframe" to named vector
    ds_df <- district_seats_df[order(district_seats_df[[1]]),]
    district_seats <- ds_df[,2]
    names(district_seats) <- ds_df[,1]

    # Biproportional Apportionment
    m = biproportional(votes_matrix, district_seats, quorum_districts, quorum_total)
    seats_df = pivot_to_df(m, new_seats_col)

    stopifnot(nrow(votes_df) <= nrow(seats_df))

    return_df = merge(votes_df,
                      seats_df,
                      sort = FALSE,
                      by = colnames(votes_df)[1:2])
    class(return_df) <- class(votes_df)
    attributes(return_df)$divisors <- attributes(m)$divisors
    return(return_df)
}

#' Biproportional apportionment
#'
#' Parties failing to reach at least one quorum cannot get seats.
#'
#' @param votes_matrix Vote count matrix with votes by party in rows
#'                     and votes by district in columns and
#' @param seats_per_district vector defining the number of seats per district.
#'                           Must be the same length as ncol(votes_matrix).
#' @inheritParams biprop_quorum
#'
#' @examples
#' votes_df = unique(zug2018[c("list_id", "entity_id", "list_votes")])
#' votes_matrix = pivot_to_matrix(votes_df)
#' votes_matrix
#' #>        entity_id
#' #> list_id  1701 1702 1703 1704 1705 1706 1707 1708 1709 1710  1711
#' #>       1  2993    0    0    0    0    0    0    0    0    0     0
#' #>       2  8108 4687 1584  531  279  477 2363 3860 1481   91 22023
#' #>       3 19389 9334 4807 1946  396 2844 3523 4702 3310  812 21343
#' #>       4 14814 6691 4005  826  379 1654 2842 2624 2713  461 33789
#' #>       5  4486 2270  621  198    0  361  728  465  925    0 10131
#' #>       6 15695 4705 1750   84    0   51  627 1106 1563  302 21794
#' #>       7 21298 8178 2875 1336  399 1450 3715 2610 4063  344 26798
#'
#' distr_df = unique(zug2018[c("entity_id", "election_mandates")])
#' district_seats = setNames(distr_df$election_mandates, distr_df$entity_id)
#' district_seats
#' #> 1701 1702 1703 1704 1705 1706 1707 1708 1709 1710 1711
#' #>   15   10    6    3    2    4    7    6    6    2   19
#'
#' biproportional(votes_matrix, district_seats, 0.05, 0.03)
#' #>        entity_id
#' #> list_id 1701 1702 1703 1704 1705 1706 1707 1708 1709 1710 1711
#' #>       1    1    0    0    0    0    0    0    0    0    0    0
#' #>       2    2    1    1    0    0    0    1    2    1    0    3
#' #>       3    3    3    2    1    0    2    2    2    1    1    3
#' #>       4    2    2    1    1    1    1    2    1    1    1    4
#' #>       5    1    1    0    0    0    0    0    0    0    0    2
#' #>       6    3    1    1    0    0    0    0    0    1    0    3
#' #>       7    3    2    1    1    1    1    2    1    2    0    4
#'
#' @export
biproportional = function(votes_matrix,
                          seats_per_district,
                          quorum_districts = 0,
                          quorum_total = 0) {
    if(!is.matrix(votes_matrix)) {
        stop(deparse(substitute(votes_matrix)), " must be a matrix")
    }
    if(ncol(votes_matrix) != length(seats_per_district)) {
        stop(deparse(substitute(votes_matrix)),
             " needs to have districts as columns and parties as rows")
    }
    if(!is.null(colnames(votes_matrix))) {
        if(!all(colnames(votes_matrix) == names(seats_per_district))) {
            stop(deparse(substitute(seats_per_district)),
                 " needs to have the same names as the columns in ",
                 deparse(substitute(votes_matrix)))
        }
    }

    # Quorum
    votes_matrix <- biprop_quorum(votes_matrix, )

    # upper apportionment (Oberzuteilung)
    seats_party = upper_apportionment(votes_matrix, seats_per_district)

    # lower apportionment (Unterzuteilung)
    seats_matrix = lower_apportionment(votes_matrix, seats_per_district, seats_party)
    class(seats_matrix) <- append(class(seats_matrix), "proporz_matrix")
    return(seats_matrix)
}

#' Filter a votes_matrix
#'
#' Parties failing to reach at least one quorum have their votes set to zero.
#'
#' @param votes_matrix matrix containing votes a parties got in districts.
#'                     Parties by row and districts by column.
#' @param quorum_districts Vote threshold a party must reach in _at least_ one
#'                         district. Used as quota of total votes within a
#'                         district if less than 1 otherwise as number of votes.
#' @param quorum_total Vote threshold a party must reach for all votes cast.
#'                     Used as quota of total votes within a district if less
#'                     than 1 otherwise as number of votes.
#' @export
biprop_quorum = function(votes_matrix, quorum_districts = 0, quorum_total = 0) {
    q_distr = votes_matrix/colSums(votes_matrix) >= quorum_districts
    q_total = votes_matrix/sum(votes_matrix) >= quorum_total

    list_missed_quorum = rowSums(q_distr | q_total) == 0
    votes_matrix[which(list_missed_quorum),] <- 0
    return(votes_matrix)
}

# same as sainte-lague
upper_apportionment = function(M, n_seats_district, upper_app_fun) {
    M_seats_district = matrix(rep(n_seats_district, nrow(M)),
                              byrow = TRUE, ncol = length(n_seats_district))

    weighted_party_votes = rowSums(M/M_seats_district)
    n_seats = sum(n_seats_district)

    hzv(weighted_party_votes, n_seats, 0.5)
}

# Calculate lower apportionment
lower_apportionment = function(M, seats_district, seats_party) {
    stopifnot(sum(M %% 1) == 0,
              (seats_district %% 1) == 0,
              (seats_party %% 1) == 0)
    # divisor parties
    dP = rep(1, nrow(M))
    dP.min = rep(0.5, nrow(M))
    dP.max = rep(1.5, nrow(M))
    # divisor districts
    dD = round(colSums(M)/seats_district)
    dD.min = floor(colSums(M)/(seats_district+1) / max(dP.max))
    dD.max = ceiling(colSums(M)/(seats_district-1) / min(dP.min))

    # calculate raw seat matrix
    # acesses function environment variables div_distr and div_party
    m. = function(.M, .div_distr, .div_party) {
        M_district = matrix(rep(.div_distr, nrow(.M)), byrow = TRUE, nrow = nrow(.M))
        M_party = matrix(rep(.div_party, ncol(.M)), byrow = FALSE, nrow = nrow(.M))
        .M/M_district/M_party
    }

    # convenience funtions to round and summarise
    mc = function(.M,.d,.p) colSums(round(m.(.M,.d,.p)))
    mr = function(.M,.d,.p) rowSums(round(m.(.M,.d,.p)))

    which.min0 = function(x) {
        x[which(x == 0)] <- max(x)
        if(all(x == max(x))) return(c())
        which.min(x)
    }
    which.max0 = function(x) {
        x[which(x == 0)] <- min(x)
        if(all(x == min(x))) return(c())
        which.max(x)
    }

    while(!all(c(mc(M,dD,dP) == seats_district, mr(M,dD,dP) == seats_party))) {
        # change party divisors
        row_decr = which.min0(mr(M,dD,dP) - seats_party)
        if(length(row_decr) == 1) {
            dP[row_decr] <- find_divisor(
                M[row_decr,]/dD,
                dP[row_decr], dP.min[row_decr],
                seats_party[row_decr])
        }

        row_incr = which.max0(mr(M,dD,dP) - seats_party)
        if(length(row_incr) == 1) {
            dP[row_incr] <- find_divisor(
                M[row_incr,]/dD,
                dP[row_incr], dP.max[row_incr],
                seats_party[row_incr])
        }

        # change district divisors
        col_decr = which.min0(mc(M,dD,dP) - seats_district)
        if(length(col_decr) == 1) {
            dD[col_decr] <- find_divisor(
                M[,col_decr]/dP,
                dD[col_decr], dD.min[col_decr],
                seats_district[col_decr])
        }

        col_incr = which.max0(mc(M,dD,dP) - seats_district)
        if(length(col_incr) == 1) {
            dD[col_incr] <- find_divisor(
                M[,col_incr]/dP,
                dD[col_incr], dD.max[col_incr],
                seats_district[col_incr])
        }
    }

    output = round(m.(M, dD, dP))
    attributes(output)$divisors <- list()
    attributes(output)$divisors$districts <- dD
    names(attributes(output)$divisors$districts) <- colnames(M)
    attributes(output)$divisors$parties <- dP
    names(attributes(output)$divisors$parties) <- rownames(M)
    return(output)
}

find_divisor = function(votes,
                        divisor_from, divisor_to,
                        target_seats) {
    stopifnot(length(target_seats) == 1)

    fun = function(divisor) {
        target_seats - sum(round(votes/divisor))
    }

    divisor_range = sort(c(divisor_from, divisor_to))

    # Divisors should be within votes/(seats-1) and votes/(seats+1).
    # It might be necessary to increase the search range given that
    # party divisors are applied as well
    while(fun(divisor_range[1]) > 0 && fun(divisor_range[2]) > 0) {
        # expand lower limit
        divisor_range[1] <- divisor_range[1]/2
    }
    while(fun(divisor_range[1]) < 0 && fun(divisor_range[2]) < 0) {
        # expand upper limit
        divisor_range[2] <- divisor_range[2]*2
    }

    bisect(fun, divisor_range[1], divisor_range[2])
}

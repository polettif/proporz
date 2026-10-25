is_quorum_param_list = function(.quorum) {
    if(!is.list(.quorum) || is.data.frame(.quorum)) {
        return(FALSE)
    }
    quorum_names = c("votes_any_district", "votes_total",
                     "any_district_voters", "total_voters")
    if(!all(names(.quorum) %in% quorum_names)) {
        return(FALSE)
    }
    if(length(setdiff(c("ANY", "ALL"), attributes(.quorum)[["type"]])) != 1) {
        return(FALSE)
    }
    return(all(vapply(.quorum, is.atomic, logical(1))))
}

is_quorum_vector = function(.quorum) {
    if(length(.quorum) == 0 || !is.atomic(.quorum) || !is.logical(.quorum) || anyNA(.quorum)) {
        return(FALSE)
    }
    return(TRUE)
}

assert_viable_quorum = function(quorum) {
    if(!is_quorum_param_list(quorum) && !is_quorum_vector(quorum)) {
        stop("Quorum must be a logical vector or a quorum parameter list (see ?quorum_functions)",
             call. = FALSE)
    }
    invisible(TRUE)
}

check_quorum_param_vector = function(quorum) {
    if(!(is_num1(quorum) && quorum >= 0)) {
        stop("Quorum parameter must be a single number >= 0",
             call. = FALSE)
    }
    invisible(TRUE)
}

check_quorum_params_num1 = function(quorum_params) {
    for(q_name in names(quorum_params)) {
        if(!is_num1(quorum_params[[q_name]]) || quorum_params[[q_name]] <= 0) {
            stop("`", q_name, "` must be a single number > 0", call. = FALSE)

        }
    }
}

missing_arg_error = function(a, n) {
    stop("Argument `", a, "` is missing (needed for `", n, "`)", call. = FALSE)

}

# can't catch total/any_district via ... because partial argument
# matching (total to votes_total) happens first, dots are not even evaluated
catch_replaced_quorum_params = function(total, any_district) {
    if(!is.null(total)) {
        stop("The argument `total` is deprecated, ",
             "please specify total_voters (recommended) or `votes_total`",
             call. = FALSE)
    }
    if(!is.null(any_district)) {
        stop("The argument `any_district` is deprecated, ",
             "please specify `votes_any_district` (recommended) or `any_district_voters`",
             call. = FALSE)
    }
}

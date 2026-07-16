# simpler and marginally faster version of stopifnot
assert = function(check) {
    if(length(check) == 0 || !isTRUE(all(check))) {
        .x = deparse(substitute(check), width.cutoff = 120L)
        .x <- paste(.x, collapse = "")
        stop(.x, " is not TRUE", call. = FALSE)
    }
    invisible(TRUE)
}

has_duplicates_or_NA = function(x) {
    anyNA(x) || anyDuplicated(x) != 0
}

assert_no_duplicates = function(vec) {
    assert(anyDuplicated(vec) == 0L)
    invisible(TRUE)
}

collapse_names = function(x, x_names = NULL) {
    if(is.logical(x)) {
        x <- which(x)
    }
    if(!is.null(x_names)) {
        assert(is.numeric(x))
        x <- x_names[x]
    }

    if(is.character(x)) {
        out = paste(x, collapse = "', '")
        out <- paste0("'", out, "'")
    } else {
        out = paste(x, collapse = ", ")
    }

    return(out)
}

trim_varname = function(x) {
    x <- x[1]
    if(nchar(x) > 33) {
        return(paste0(substring(x, 1, 30), "..."))
    }
    return(x)
}

num_word = function(singular, plural, i) {
    if(is.logical(i)) {
        i <- which(i)
    }
    if(length(i) == 1L) {
        return(singular)
    }
    return(plural)
}

# divide x by div and assign 0 for infinite or nan values
div0 = function(x, div) {
    d = x/div
    d[is.nan(d) | is.infinite(d) | is.na(d)] <- 0
    d
}

# type checks, single value objects, no NAs
is_bool1 = function(x) {
    length(x) == 1L && !is.na(x) && is.logical(x) # length(NULL) is 0
}

is_num1 = function(x) {
    length(x) == 1L && !is.na(x) && is.numeric(x)
}

is_char1 = function(x) {
    length(x) == 1L && !is.na(x) && is.character(x)
}

assert_bool1 = function(x) {
    if(!is_bool1(x)) {
        stop("`", deparse(substitute(x)), "` must be TRUE or FALSE", call. = FALSE)
    }
    invisible(TRUE)
}

assert_char1 = function(x) {
    if(!is_char1(x)) {
        stop("`", deparse(substitute(x)), "` must be a single string", call. = FALSE)
    }
    invisible(TRUE)
}

assert_num1 = function(x) {
    if(!is_num1(x)) {
        stop("`", deparse(substitute(x)), "` must be a single number", call. = FALSE)
    }
    invisible(TRUE)
}

equal_names = function(x, y) {
    setequal(x, y) && !has_duplicates_or_NA(x) && !has_duplicates_or_NA(y)
}

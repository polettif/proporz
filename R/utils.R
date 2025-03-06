# simpler and marginally faster version of stopifnot
assert = function(check) {
    if(!all(check)) {
        .x = deparse(substitute(check))
        stop(.x, " is not TRUE", call. = FALSE)
    }
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

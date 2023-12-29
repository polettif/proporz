#' Round x up if x-floor(x) >= threshold
#' @param x numeric value
#' @param threshold threshold in 0..1
ceil_at = function(x, threshold) {
    stopifnot(!is.na(threshold))
    if(any(x < 0)) {
        stop("x cannot be negative")
    }
    values = c(x)

    if(is.numeric(threshold)) {
        if(threshold < 0 || threshold > 1) {
            stop("Threshold argument must be in [0,1]")
        }
        threshold <- floor(values) + threshold
    } else if(threshold == "harmonic") {
        threshold <- threshold_harmonic(values)
    } else if(threshold == "geometric") {
        threshold <- threshold_geometric(values)
    } else {
        stop('Numeric value, "harmonic" or "geometric" expected for threshold argument')
    }

    ceiled = ceiling(values)
    floor_index = values < threshold
    ceiled[floor_index] <- floor(values)[floor_index]

    if(is.matrix(x)) {
        ceiled_matrix = matrix(ceiled, nrow = nrow(x), ncol = ncol(x))
        return(ceiled_matrix)
    } else {
        return(ceiled)
    }
}

get_round_function = function(method_name) {
    m = get_apport_method(method_name)
    method_thresholds = list(
        "ceiling" = 0,
        "round" = 0.5,
        "floor" = 1,
        "harmonic" = "harmonic",
        "geometric" = "geometric"
    )

    function(x) ceil_at(x, method_thresholds[[m]])
}

threshold_harmonic = function(x) {
    x_ceil = ceiling(x)
    x_floor = floor(x)

    harmonic = (2*x_ceil*x_floor)/(x_ceil + x_floor)
    harmonic[x == 0] <- 0  # 0+eps has to be rounded to 1
    return(harmonic)
}

threshold_geometric = function(x) {
    x_ceil = ceiling(x)
    x_floor = floor(x)

    geometric = sqrt(x_ceil*x_floor)
    return(geometric)
}

#' Rounding with predefined thresholds
#'
#' Round `x` up if `x-floor(x) >= threshold`, otherwise round down.
#'
#' @param x numeric vector >= 0 (`NaN` is not supported)
#' @param threshold threshold in \[0,1\] or "harmonic"/"geometric" to use
#'   harmonic or geometric mean thresholds
#'
#' @returns the rounded vector
#'
#' @examples
#' ceil_at(c(0.5, 1.5, 2.49, 2.5, 2.51), 0.5)
#' # compare to
#' round(c(0.5, 1.5, 2.49, 2.5, 2.51))
#'
#' ceil_at(c(1.45, 2.45, 3.45), 0) # like floor()
#' ceil_at(c(1.45, 2.45, 3.45, 0.2), "geometric")
#' @export
ceil_at = function(x, threshold) {
    assert(length(threshold) == 1 && !is.na(threshold))
    assert(all(!is.na(x)) && all(is.numeric(x)) && all(x >= 0))
    values = c(x)

    if(is.numeric(threshold)) {
        if(threshold < 0 || threshold > 1) {
            stop("Threshold argument must be in [0,1].", call. = FALSE)
        }
        threshold <- floor(values) + threshold
    } else if(threshold == "harmonic") {
        threshold <- threshold_harmonic(values)
    } else if(threshold == "geometric") {
        threshold <- threshold_geometric(values)
    } else {
        stop('Numeric value, "harmonic" or "geometric" expected for threshold argument.',
             call. = FALSE)
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
    method_thresholds = list(
        "divisor_ceiling" = 0,
        "divisor_round" = 0.5,
        "divisor_floor" = 1,
        "divisor_harmonic" = "harmonic",
        "divisor_geometric" = "geometric"
    )

    function(x) ceil_at(x, method_thresholds[[method_name]])
}


threshold_harmonic = function(x) {
    x_ceil = ceiling(x)
    x_floor = floor(x)

    harmonic = seq_harmonic(x_ceil, x_floor)
    harmonic[x == 0] <- 0  # 0+eps has to be rounded to 1
    return(harmonic)
}

seq_harmonic = function(nn, nn1 = nn-1) {
    (2*nn*(nn1))/(nn + (nn1))
}

threshold_geometric = function(x) {
    x_ceil = ceiling(x)
    x_floor = floor(x)

    geometric = seq_geometric(x_ceil, x_floor)
    return(geometric)
}

seq_geometric = function(nn, nn1 = nn-1) {
    sqrt((nn1)*nn)
}

ceil_at = function(x, threshold) {
	if(any(x < 0)) {
		stop("x cannot be negative")
	}
	x_ceil = ceiling(x)
	x_floor = floor(x)

	if(is.numeric(threshold)) {
		if(threshold < 0 || threshold > 1) {
			stop("Threshold argument must be in [0,1]")
		}
	} else if(threshold == "harmonic") {
		threshold <- (2*x_ceil*x_floor)/(x_ceil + x_floor)
	} else if(threshold == "geometric") {
		threshold <- sqrt(x_ceil*x_floor)
	} else {
		stop('Numeric value, "harmonic" or "geometric" ',
			 'expected for threshold argument')
	}
	x_ceiled = x_ceil

	x_dec = x-x_floor
	floor_index = which(x_dec < threshold)
	x_ceiled[floor_index] <- x_floor[floor_index]

	thr_delta = abs(threshold-x_dec)
	eps_index = which(thr_delta < .Machine$double.eps)
	x_ceiled[eps_index] <- x_ceil[eps_index]

	x_ceiled
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

#' Proportional apportionment
#'
#' Calculate seat apportionment for legislative bodies.
#'
#' @param votes numeric vector with number of votes for each party
#' @param n_seats total number of seats
#' @param method Apportionment method to use, as character. Not case sensitive. See details.
#' @param quorum Vote threshold a party must reach. Used as quota of total
#'               votes within a district if less than 1 otherwise as number
#'               of votes.
#'
#' @details The following methods are available: `r .doc_proporz_methods()`
#'
#' @returns The number of seats per party as a vector
#'
#' @note Seats can also be apportioned among regions instead of parties. The
#'       parameter `votes` is then normally used with census data (e.g.
#'       population counts).
#'
#' @examples
#' votes = c("Party A" = 651, "Party B" = 349, "Party C" = 50)
#'
#' proporz(votes, 10, "sainte-lague")
#'
#' proporz(votes, 10, "hill-huntington")
#'
#' proporz(votes, 10, "hill-huntington", quorum = 0.05)
#'
#' proporz(votes, 10, "jefferson", quorum = 70)
#'
#'@export
proporz = function(votes, n_seats, method, quorum = 0) {
    proporz_method = get_method_implementation(method)
    proporz_func = match.fun(proporz_method)
    proporz_func(votes, n_seats, quorum)
}

#' List of method names and their implementation
#'
#' Names can be used in [proporz()] or [biproporz()], the list entries
#' denote the name of the implementation function.
#' @returns Named list of methods
#' @keywords internal
proporz_methods = list(
    "d'hondt" = "divisor_floor",
    "jefferson" = "divisor_floor",
    "hagenbach-bischoff" = "divisor_floor",
    "sainte-lague" = "divisor_round",
    "webster" = "divisor_round",
    "adams" = "divisor_ceiling",
    "dean" = "divisor_harmonic",
    "huntington-hill" = "divisor_geometric",
    "hill-huntington" = "divisor_geometric",
    "hare-niemeyer" = "largest_remainder_method",
    "hamilton" = "largest_remainder_method",
    "vinton" = "largest_remainder_method",
    "floor" = "divisor_floor",
    "round" = "divisor_round",
    "ceiling" = "divisor_ceiling",
    "harmonic" = "divisor_harmonic",
    "geometric" = "divisor_geometric",
    "largest_remainder_method" = "largest_remainder_method",
    "divisor_floor" = "divisor_floor",
    "divisor_round" = "divisor_round",
    "divisor_ceiling" = "divisor_ceiling",
    "divisor_harmonic" = "divisor_harmonic",
    "divisor_geometric" = "divisor_geometric"
)

get_method_implementation = function(method_name) {
    method_name <- tolower(method_name)
    if(!method_name %in% names(proporz_methods)) {
        stop("Unknown apportion method: ", method_name, ".\nAvailable: ",
             paste0(names(proporz_methods), collapse=", "), call. = FALSE)
    }
    return(proporz_methods[[method_name]])
}

# function to create the list of method names for the proporz documentation
.doc_proporz_methods = function(only_divisor_methods = FALSE) { # nocov start
    doc = c("\\itemize{")
    implementation_list = unique(unlist(proporz_methods))
    if(only_divisor_methods) {
        implementation_list <- implementation_list[grepl("divisor_", implementation_list)]
    }
    for(implementation in implementation_list) {
        method_names = names(proporz_methods[proporz_methods == implementation])
        method_names <- method_names[!grepl("divisor_", method_names)]
        if(only_divisor_methods) {
            method_names <- method_names[!method_names %in% c("floor", "geometric", "harmonic", "round", "ceiling")]
        }
        method_names = paste0(method_names, collapse = ", ")
        doc[length(doc)+1] <- paste0("    \\item{", method_names,
                                     ": [", implementation, "()]}")
    }
    doc[length(doc)+1] <- "}"
    return(paste(doc, collapse = "\n"))
} # nocov end

#' List of all apportion methods
#'
#' Names can be used in [proporz()] or [biproportional()], the list entries
#' denote the actual implementation.
#'
#' @details
#' \code{list(
#' "divisor_floor" = "floor",
#' "d'hondt" = "floor",
#' "jefferson" = "floor",
#' "divisor_round" = "round",
#' "sainte-lague" = "round",
#' "webster" = "round",
#' "divisor_ceiling" = "ceiling",
#' "adams" = "ceiling",
#' "dean" = "harmonic",
#' "huntington-hill" = "geometric",
#' "hill-huntington" = "geometric",
#' "hare-niemeyer" = "quota_largest_remainder",
#' "hamilton" = "quota_largest_remainder",
#' "vinton" = "quota_largest_remainder",
#' "hagenbach-bischoff" = "floor",
#' "ceiling" = "ceiling",
#' "round" = "round",
#' "floor" = "floor",
#' "harmonic" = "harmonic",
#' "geometric" = "geometric",
#' "quota_largest_remainder" = "quota_largest_remainder"
#' )}
#'
#' @returns Named list of methods
#' @export
apport_methods = list(
    "divisor_floor" = "floor",
    "d'hondt" = "floor",
    "jefferson" = "floor",
    "divisor_round" = "round",
    "sainte-lague" = "round",
    "webster" = "round",
    "divisor_ceiling" = "ceiling",
    "adams" = "ceiling",
    "dean" = "harmonic",
    "huntington-hill" = "geometric",
    "hill-huntington" = "geometric",
    "hare-niemeyer" = "quota_largest_remainder",
    "hamilton" = "quota_largest_remainder",
    "vinton" = "quota_largest_remainder",
    "hagenbach-bischoff" = "floor",
    "ceiling" = "ceiling",
    "round" = "round",
    "floor" = "floor",
    "harmonic" = "harmonic",
    "geometric" = "geometric",
    "quota_largest_remainder" = "quota_largest_remainder"
)

get_apport_method = function(method_name) {
    method_name <- tolower(method_name)
    if(!method_name %in% names(apport_methods)) {
        stop("Unknown apportion method: ", method_name, ".\nAvailable: ",
             paste0(names(apport_methods), collapse=", "))
    }

    apport_methods[[method_name]]
}

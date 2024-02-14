#' Example election data from Zug Switzerland (2018)
"zug2018"

#' Finnish Parliamentary Elections Data (2019)
#'
#' Example data from the 2019 Finnish parliamentary elections. The data has been
#' cleaned up and only contains information relevant for this package.
#'
#' @format List containing two data.frames:
#' \itemize{
#'       \item{`votes_df` containing the number of votes for each party and district.
#'             229 rows, 3 columns (`party_name`, `district_name`, `votes`)}
#'       \item{`district_seats_df` with the number of seats per district.
#'             12 rows, 2 columns (`district_name`, `seats`)}
#' }
#'
#' @source \url{https://tulospalvelu.vaalit.fi/EKV-2019/en/ladattavat_tiedostot.html}
#' @examples
#' finland2019$seats_df
#'
#' head(finland2019$votes_df)
#' @keywords data
"finland2019"

#' Prepared and cleaned vote matrices and district seats data
"biproporz_examples"

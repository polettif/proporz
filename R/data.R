#' Election Data for the Cantonal Council of Zug (2018)
#'
#' Example election data from the 2018 election for the cantonal council
#' of Zug (Kantonsrat) in Switzerland.
#'
#' @source Kanton Zug (01.07.2022, 10:27:58). Kantonsratswahl 2018 (CSV).
#'         \url{https://wab.zug.ch/elections/kantonsratswahl-2018/data-csv}
#' @keywords data
"zug2018"

#' Election Data for the Cantonal Council of Uri (2020)
#'
#' Example election data from the 2020 election for the cantonal council
#' of Uri (Landrat) in Switzerland. The data has been extracted from the report
#' "Landratswahlen 2020: Statistische Auswertung".
#'
#' @format List containing:
#' \itemize{
#'       \item{`votes_matrix` the number of votes for each party and district
#'             (4 rows, 4 columns)}
#'       \item{`seats_vector` with the number of seats per district
#'             (length 4)}
#' }
#'
#' @source \url{https://www.ur.ch/abstimmungen/termine/9322}
#' @keywords data
"uri2020"

#' Election Data for the Cantonal Council of Grisons (2022)
#'
#' Example election data from the 2020 election for the cantonal council
#' of Grisons (Grossrat, Cussel grond, Gran Consiglio) in Switzerland. The data
#' has been extracted from the document "Protokoll der Grossratswahlen vom 15.
#' Mai 2022".
#'
#' @format List containing:
#' \itemize{
#'       \item{`votes_matrix` the number of votes for each district and party
#'             (39 rows, 5 columns). Note that districts are in rows and parties
#'             in columns, contrary to convention within this package.}
#'       \item{`district_seats_df` with the number of seats per district
#'             (37 rows, 2 columns)}
#' }
#'
#' @source \url{https://www.gr.ch/DE/publikationen/abstimmungenwahlen/Grossratswahlen-2022/resultate/Seiten/Resultate.aspx}
#' @keywords data
"grisons2022"

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
#' finland2019$district_seats_df
#'
#' head(finland2019$votes_df)
#' @keywords data
"finland2019"

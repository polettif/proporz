% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyapp.R
\name{run_app}
\alias{run_app}
\title{Use biproportional apportionment interactively in a shiny app}
\usage{
run_app(votes_matrix = NULL, district_seats = NULL)
}
\arguments{
\item{votes_matrix}{optional votes_matrix to load upon start}

\item{district_seats}{optional district_seats to load upon start}
}
\description{
Use biproportional apportionment interactively in a shiny app
}
\examples{
if(interactive()){
    # You need to have shiny and shinyMatrix installed to run the app
    run_app()

    # It's possible to load a matrix with the app
    run_app(biproporz_examples$uri_2020$votes, biproporz_examples$uri_2020$seats)
}
}
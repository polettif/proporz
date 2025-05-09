% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proporz.R
\name{proporz}
\alias{proporz}
\title{Proportional apportionment}
\usage{
proporz(votes, n_seats, method, quorum = 0)
}
\arguments{
\item{votes}{numeric vector with number of votes for each party}

\item{n_seats}{total number of seats}

\item{method}{Apportionment method to use, as character. Not case sensitive. See details.}

\item{quorum}{Vote threshold a party must reach. Used as fraction of total
votes within if less than 1 otherwise as number
of votes.}
}
\value{
The number of seats per party as a vector
}
\description{
Calculate seat apportionment for legislative bodies.
}
\details{
The following methods are available: \itemize{
\item{d'hondt, jefferson, hagenbach-bischoff, floor: \code{\link[=divisor_floor]{divisor_floor()}}}
\item{sainte-lague, webster, round: \code{\link[=divisor_round]{divisor_round()}}}
\item{adams, ceiling: \code{\link[=divisor_ceiling]{divisor_ceiling()}}}
\item{dean, harmonic: \code{\link[=divisor_harmonic]{divisor_harmonic()}}}
\item{huntington-hill, hill-huntington, geometric: \code{\link[=divisor_geometric]{divisor_geometric()}}}
\item{hare-niemeyer, hamilton, vinton, largest_remainder_method: \code{\link[=largest_remainder_method]{largest_remainder_method()}}}
}
}
\note{
Seats can also be apportioned among regions instead of parties. The
parameter \code{votes} is then normally used with census data (e.g.
population counts).
}
\examples{
votes = c("Party A" = 651, "Party B" = 349, "Party C" = 50)

proporz(votes, 10, "sainte-lague")

proporz(votes, 10, "hill-huntington")

proporz(votes, 10, "hill-huntington", quorum = 0.05)

proporz(votes, 10, "jefferson", quorum = 70)

}

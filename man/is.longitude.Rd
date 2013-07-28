\name{is.longitude}
\alias{is.longitude}
\title{Validate a longitude value}
\usage{
  is.longitude(longitude)
}
\arguments{
  \item{longitude}{a longitude coordinate value}
}
\description{
  Return a TRUE value is the input parameter is a valid
  longitude value, FALSE otherwise
}
\examples{
#' # return a true value
is.longitude(40)

# return a false value
is.longitude(200)
}


\name{is.latitude}
\alias{is.latitude}
\title{Validate a latitude value}
\usage{
  is.latitude(latitude)
}
\arguments{
  \item{latitude}{a latitude coordinate value}
}
\description{
  Return a TRUE value is the input parameter is a valid
  latitude value, FALSE otherwise
}
\examples{
# return a true value
is.latitude(40)

# return a false value
is.latitude(100)
}


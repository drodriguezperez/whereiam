\name{vincentyDistance}
\alias{vincentyDistance}
\alias{vincentyDistance.Coordinate}
\alias{vincentyDistance.default}
\title{Calculate distance between two points}
\usage{
  vincentyDistance(...)

  \method{vincentyDistance}{default} (latitude1,
    longitude1, latitude2, longitude2, units = "km",
    maxIter = 100, convCrite = 1e-12, ...)

  \method{vincentyDistance}{Coordinate} (coordinate1,
    coordinate2, units = "km", maxIter = 100,
    convCrite = 1e-12, ...)
}
\arguments{
  \item{latitude1}{the first latitude coordinate}

  \item{longitude1}{the first longitude coordinate}

  \item{latitude2}{the second latitude coordinate}

  \item{longitude2}{the second longitude coordinate}

  \item{units}{a string with the distance units (default
  kilometers)}

  \item{coordinate1}{the first coordinate class variable}

  \item{coordinate2}{the second coordinate class variable}

  \item{maxIter}{maximum number of iterations in the
  calculation}

  \item{convCrite}{convergence criteria for the
  calculation}

  \item{...}{other arguments}
}
\description{
  The function calculates distances between the two points
  using the Vincenty's formulae.
}
\examples{
cord1    <- Coordinate(43, -8)
cord2    <- Coordinate(42, -7)
distance <- vincentyDistance(cord1, cord2)
}


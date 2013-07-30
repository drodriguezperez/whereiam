\name{sphericalDistance}
\alias{sphericalDistance}
\alias{sphericalDistance.Coordinate}
\alias{sphericalDistance.default}
\title{Calculate distance between two points}
\usage{
  sphericalDistance(...)

  \method{sphericalDistance}{default} (latitude1,
    longitude1, latitude2, longitude2, units = "km", ...)

  \method{sphericalDistance}{Coordinate} (coordinate1,
    coordinate2, units = "km", ...)
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

  \item{...}{other arguments}
}
\description{
  The function calculates distances between the two points
  using the Spherical Law of Cosines formula.
}
\examples{
cord1    <- Coordinate(43, -8)
cord2    <- Coordinate(42, -7)
distance <- sphericalDistance(cord1, cord2)
}


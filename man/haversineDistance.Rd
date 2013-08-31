\name{haversineDistance}
\alias{haversineDistance}
\alias{haversineDistance.Coordinate}
\alias{haversineDistance.default}
\title{Calculate distance between two points along great circle using the
haversine formulae}
\usage{
  haversineDistance(...)

  \method{haversineDistance}{default} (latitude1,
    longitude1, latitude2, longitude2, units = "km", ...)

  \method{haversineDistance}{Coordinate} (coordinate1,
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
  The function calculates great-circle distances between
  the two points using the "Haversine" formulae. This is
  the shortest distance over the earth's surface ignoring
  the geographic shape like hills and vales.
}
\examples{
cord1    <- Coordinate(43, -8)
cord2    <- Coordinate(42, -7)
distance <- haversineDistance(cord1, cord2)
}
\references{
  http://www.movable-type.co.uk/scripts/latlong.html
}


\name{rhumbDistance}
\alias{rhumbDistance}
\alias{rhumbDistance.Coordinate}
\alias{rhumbDistance.default}
\title{Calculate distance between two points along a rhumb line}
\usage{
  rhumbDistance(...)

  \method{rhumbDistance}{default} (latitude1, longitude1,
    latitude2, longitude2, units = "km", ...)

  \method{rhumbDistance}{Coordinate} (coordinate1,
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
  along a rhumb line. The rhumb lines are path of constant
  bearing and crosses all meridians at the same angle. The
  rhumb lines are generally longer than great-circle
  routes.
}
\examples{
cord1    <- Coordinate(43, -8)
cord2    <- Coordinate(42, -7)
distance <- rhumbDistance(cord1, cord2)
}
\references{
  http://www.movable-type.co.uk/scripts/latlong.html
}


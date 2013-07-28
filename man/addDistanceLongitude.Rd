\name{addDistanceLongitude}
\alias{addDistanceLongitude}
\alias{addDistanceLongitude.Coordinates}
\alias{addDistanceLongitude.default}
\title{Add distance in the longitude direction to a coordinate}
\usage{
  addDistanceLongitude(...)

  \method{addDistanceLongitude}{default} (latitude,
    longitude, distance, units = "km", ...)

  \method{addDistanceLongitude}{Coordinates} (coordinate,
    distance, units = "km", ...)
}
\arguments{
  \item{latitude}{a latitude coordinate}

  \item{longitude}{a longitude coordinate}

  \item{distance}{the distance to add in km or miles}

  \item{units}{a string with the distance units (default
  kilometers)}

  \item{coordinate}{a coordinate class}

  \item{...}{other arguments}
}
\value{
  a Coordinates object with the new coordinates
}
\description{
  Add distance in the longitude direction to the indicate
  coordinate. The distance can be indicated on kilometers
  or miles
}
\examples{
# Add 10 kilometers to the east to a position
cord <- Coordinates(43, -8)
cord <- addDistanceLongitude(cord, 10)
}


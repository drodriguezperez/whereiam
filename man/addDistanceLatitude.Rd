\name{addDistanceLatitude}
\alias{addDistanceLatitude}
\alias{addDistanceLatitude.Coordinates}
\alias{addDistanceLatitude.default}
\title{Add distance in the latitude direction to a coordinate}
\usage{
  addDistanceLatitude(...)

  \method{addDistanceLatitude}{default} (latitude,
    longitude, distance, units = "km", ...)

  \method{addDistanceLatitude}{Coordinates} (coordinate,
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
  Add distance in the latitude direction to the indicate
  coordinate. The distance can be indicated on kilometers
  or miles
}
\examples{
# Add 10 kilometers to the north to a position
cord <- Coordinates(43, -8)
cord <- addDistanceLatitude(cord, 10)
}


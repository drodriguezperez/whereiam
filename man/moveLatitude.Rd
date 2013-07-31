\name{moveLatitude}
\alias{moveLatitude}
\alias{moveLatitude.Coordinate}
\alias{moveLatitude.default}
\title{Add distance in the latitude direction to a coordinate}
\usage{
  moveLatitude(...)

  \method{moveLatitude}{default} (latitude, longitude,
    distance, units = "km", ...)

  \method{moveLatitude}{Coordinate} (coordinate, distance,
    units = "km", ...)
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
  a Coordinate object with the new coordinates
}
\description{
  Add distance in the latitude direction to the indicate
  coordinate. The distance can be indicated on kilometers
  or miles
}
\examples{
# Add 10 kilometers to the north to a position
cord <- Coordinate(43, -8)
cord <- moveLatitude(cord, 10)
}


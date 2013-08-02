\name{destination}
\alias{destination}
\alias{destination.Coordinate}
\alias{destination.default}
\title{Destination point from start, bearing and distance along great circle}
\usage{
  destination(...)

  \method{destination}{default} (latitude, longitude, brg,
    distance, units = "km", ...)

  \method{destination}{Coordinate} (coordinate, brg,
    distance, units = "km", ...)
}
\arguments{
  \item{latitude}{a latitude coordinate}

  \item{longitude}{a longitude coordinate}

  \item{brg}{the initial bearing}

  \item{distance}{the distance to add in km or miles}

  \item{units}{a string with the distance units (default
  kilometers)}

  \item{coordinate}{a coordinate class}

  \item{...}{other argument}
}
\description{
  Obtain a destination point from a given start point,
  initial bearing, and distance
}
\examples{
# Add 10 kilometers to icord
icord <- Coordinate(43, -8)
ecord <- destination(icord, 30, 10)
}


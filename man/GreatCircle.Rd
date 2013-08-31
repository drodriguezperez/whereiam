\name{GreatCircle}
\alias{GreatCircle}
\alias{GreatCircle.Coordinate}
\alias{GreatCircle.default}
\title{Create a GreatCircle object}
\usage{
  GreatCircle(...)

  \method{GreatCircle}{default} (latitude, longitude, brg,
    ...)

  \method{GreatCircle}{Coordinate} (coordinate, brg, ...)
}
\arguments{
  \item{latitude}{a latitude coordinate}

  \item{longitude}{a longitude coordinate}

  \item{brg}{the initial bearing}

  \item{coordinate}{a coordinate class}

  \item{...}{other argument}
}
\description{
  The method create a new GreatCircle object type using the
  indicated latitude and longitude and bearing
}
\references{
  http://en.wikipedia.org/wiki/Great-circle_distance
}


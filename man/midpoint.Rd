\name{midpoint}
\alias{midpoint}
\alias{midpoint.Coordinates}
\alias{midpoint.default}
\title{Midpoint between two points}
\usage{
  midpoint(...)

  \method{midpoint}{default} (latitude1, longitude1,
    latitude2, longitude2, ...)

  \method{midpoint}{Coordinates} (coordinate1, coordinate2,
    ...)
}
\arguments{
  \item{latitude1}{the first latitude coordinate}

  \item{longitude1}{the first longitude coordinate}

  \item{latitude2}{the second latitude coordinate}

  \item{longitude2}{the second longitude coordinate}

  \item{coordinate1}{the first coordinate class variable}

  \item{coordinate2}{the second coordinate class variable}

  \item{...}{other arguments}
}
\description{
  Calculate the half-way point along a great circle path
  between the two points
}
\examples{
cord1 <- Coordinates(43, -8)
cord2 <- Coordinates(42, -7)
mcord <- midpoint(cord1, cord2)
}


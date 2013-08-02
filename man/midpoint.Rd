\name{midpoint}
\alias{midpoint}
\alias{midpoint.Coordinate}
\alias{midpoint.default}
\title{Midpoint between two points}
\usage{
  midpoint(...)

  \method{midpoint}{default} (latitude1, longitude1,
    latitude2, longitude2, line = "greatcircle", ...)

  \method{midpoint}{Coordinate} (coordinate1, coordinate2,
    line = "greatcircle", ...)
}
\arguments{
  \item{latitude1}{the first latitude coordinate}

  \item{longitude1}{the first longitude coordinate}

  \item{latitude2}{the second latitude coordinate}

  \item{longitude2}{the second longitude coordinate}

  \item{coordinate1}{the first coordinate class variable}

  \item{coordinate2}{the second coordinate class variable}

  \item{line}{a string with the line used to calculate
  greatcircle or rhumb}

  \item{...}{other arguments}
}
\description{
  Calculate the half-way point along a great circle path
  between the two points. The point can be calculated along
  a great-circle or a rhumb line.
}
\examples{
cord1 <- Coordinate(43, -8)
cord2 <- Coordinate(42, -7)

# Using great-circle
mcord <- midpoint(cord1, cord2)

# Using rhumb
mcord <- midpoint(cord1, cord2, line = 'rhumb')
}


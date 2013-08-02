\name{bearing}
\alias{bearing}
\alias{bearing.Coordinate}
\alias{bearing.default}
\title{Initial bearing between two points}
\usage{
  bearing(...)

  \method{bearing}{default} (latitude1, longitude1,
    latitude2, longitude2, line = "greatcircle", ...)

  \method{bearing}{Coordinate} (coordinate1, coordinate2,
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
  Calculate the initial bearing between two points. This
  bearing which if followed in a straight line along a
  great-circle arc will take you from the start point to
  the end point. The point can be calculated along a
  great-circle or a rhumb line.
}
\examples{
cord1 <- Coordinate(43, -8)
cord2 <- Coordinate(42, -7)

# Using great-circle
brg   <- bearing(cord1, cord2)

# Using rhumb
brg   <- bearing(cord1, cord2, line = 'rhumb')
}


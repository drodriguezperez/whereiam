\name{bearing}
\alias{bearing}
\alias{bearing.Coordinates}
\alias{bearing.default}
\title{Initial bearing between two points}
\usage{
  bearing(...)

  \method{bearing}{default} (latitude1, longitude1,
    latitude2, longitude2, ...)

  \method{bearing}{Coordinates} (coordinate1, coordinate2,
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
  Calculate the initial bearing between two points. This
  bearing which if followed in a straight line along a
  great-circle arc will take you from the start point to
  the end point
}
\examples{
cord1    <- Coordinates(43, -8)
cord2    <- Coordinates(42, -7)
distance <- bearing(cord1, cord2)
}


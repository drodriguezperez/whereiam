\name{getBearing}
\alias{getBearing}
\alias{getBearing.GreatCircle}
\alias{getBearing.default}
\title{Extract bearing from a GreatCircle}
\usage{
  getBearing(coordinate, units = "degrees")

  \method{getBearing}{default} (coordinate,
    units = "degrees")

  \method{getBearing}{GreatCircle} (coordinate,
    units = "degrees")
}
\arguments{
  \item{coordinate}{a GreatCircle class}

  \item{units}{a string with the units (degrees or
  radians)}
}
\description{
  Get the bearing from a GreatCircle object in the
  indicated units (degrees or radians)
}
\references{
  http://www.movable-type.co.uk/scripts/latlong.html
}


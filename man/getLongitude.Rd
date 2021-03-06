\name{getLongitude}
\alias{getLongitude}
\alias{getLongitude.Coordinate}
\alias{getLongitude.GreatCircle}
\alias{getLongitude.default}
\title{Extract longitude from a Coordinate}
\usage{
  getLongitude(coordinate, units = "degrees")

  \method{getLongitude}{default} (coordinate,
    units = "degrees")

  \method{getLongitude}{Coordinate} (coordinate,
    units = "degrees")

  \method{getLongitude}{GreatCircle} (coordinate,
    units = "degrees")
}
\arguments{
  \item{coordinate}{a coordinate class}

  \item{units}{a string with the units (degrees or
  radians)}
}
\description{
  Get the longitude from a Coordinate or a GreatCircle
  object in the indicated units (degrees or radians)
}


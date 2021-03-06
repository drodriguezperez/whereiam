\name{getLatitude}
\alias{getLatitude}
\alias{getLatitude.Coordinate}
\alias{getLatitude.GreatCircle}
\alias{getLatitude.default}
\title{Extract latitude from a Coordinate}
\usage{
  getLatitude(coordinate, units = "degrees")

  \method{getLatitude}{default} (coordinate,
    units = "degrees")

  \method{getLatitude}{Coordinate} (coordinate,
    units = "degrees")

  \method{getLatitude}{GreatCircle} (coordinate,
    units = "degrees")
}
\arguments{
  \item{coordinate}{a coordinate class}

  \item{units}{a string with the units (degrees or
  radians)}
}
\description{
  Get the latitude from a Coordinate or a GreatCircle
  object in the indicated units (degrees or radians)
}


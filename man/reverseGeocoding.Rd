\name{reverseGeocoding}
\alias{reverseGeocoding}
\alias{reverseGeocoding.Coordinate}
\alias{reverseGeocoding.default}
\title{Resolve the reverse geocoding of the coordinates}
\usage{
  reverseGeocoding(...)

  \method{reverseGeocoding}{default} (latitude, longitude,
    service = "google", ...)

  \method{reverseGeocoding}{Coordinate} (coordinate,
    service = "google", ...)
}
\arguments{
  \item{latitude}{the coordinates longitude}

  \item{longitude}{the coordinates longitude}

  \item{coordinate}{a coordinate class}

  \item{service}{the optional service used to obtain the
  information}

  \item{...}{other arguments}
}
\description{
  Obtains a readable address form the point location
  (latitude, longitude) using an external service.
}
\details{
  The list of services available are: * Google
}


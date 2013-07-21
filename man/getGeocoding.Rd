\name{getGeocoding}
\alias{getGeocoding}
\alias{getGeocoding.google}
\title{Resolve the reverse geocoding of the coordinates}
\usage{
  getGeocoding(latitude, longitude)

  getGeocoding.google(latitude, longitude)
}
\arguments{
  \item{latitude}{the coordinates longitude}

  \item{longitude}{the coordinates longitude}
}
\description{
  Obtains a readable address form the point location
  (latitude, longitude) using an external service.
}
\examples{
# Get information from diferent coordinates
info <- getGeocoding(42.8806027, -8.5445684)
}


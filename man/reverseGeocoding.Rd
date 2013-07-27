\name{reverseGeocoding}
\alias{reverseGeocoding}
\alias{reverseGeocoding.google}
\title{Resolve the reverse geocoding of the coordinates}
\usage{
  reverseGeocoding (latitude, longitude)

  reverseGeocoding.google(latitude, longitude)
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
info <- reverseGeocoding(42.8806027, -8.5445684)
}


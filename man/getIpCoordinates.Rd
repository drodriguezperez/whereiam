\name{getIpCoordinates}
\alias{getIpCoordinates}
\alias{getIpCoordinates.freegeoip}
\alias{getIpCoordinates.hostip}
\title{Get location coordinates from an IP}
\usage{
  getIpCoordinates(ip = getExternalIP(),
    service = "freegeoip")

  getIpCoordinates.freegeoip(ip = getExternalIP())

  getIpCoordinates.hostip(ip = getExternalIP())
}
\arguments{
  \item{ip}{an IP address for geolocation (optional default
  uses the computer)}

  \item{service}{the optional service used to obtain the
  coordinates}
}
\description{
  Returns the location coordinates of an IP using an
  external service. If an IP is indicated the function
  returns it's estimated location, otherwise it will use
  the IP of the system.
}
\examples{
# Get computer location
location <- getIpCoordinates()

# Get '10.10.10.10' location
location <- getIpCoordinates('10.10.10.10')
}


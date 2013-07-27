\name{getIpCoordinates}
\alias{getIpCoordinates}
\alias{getIpCoordinates.freegeoip}
\alias{getIpCoordinates.hostip}
\title{Get location}
\usage{
  getIpCoordinates(ip = getExternalIP())

  getIpCoordinates.freegeoip(ip = getExternalIP())

  getIpCoordinates.hostip(ip = getExternalIP())
}
\arguments{
  \item{ip}{an IP address for geolocation (optional default
  uses the computer)}
}
\description{
  Returns the location of an IP using. If an IP is
  indicated the function returns it's estimated location,
  otherwise it will use the IP of the system.
}
\examples{
# Get computer location
location <- getIpCoordinates()

# Get '10.10.10.10' location
location <- getIpCoordinates('10.10.10.10')
}


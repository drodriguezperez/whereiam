\name{getLocation}
\alias{getLocation}
\alias{getLocation.freegeoip}
\title{Get location}
\usage{
  getLocation(ip = getExternalIP())

  getLocation.freegeoip(ip = getExternalIP())
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
location <- getLocation()

# Get '10.10.10.10' location
location <- getLocation('10.10.10.10')
}


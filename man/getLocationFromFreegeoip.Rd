\name{getLocationFromFreegeoip}
\alias{getLocationFromFreegeoip}
\title{Get location from freegeoip.net}
\usage{
  getLocationFromFreegeoip(ip = getExternalIP())
}
\arguments{
  \item{ip}{an IP address for geolocation (optional default
  uses the computer)}
}
\description{
  Returns the location of an IP using freegeoip.net public
  RESTful web service. If an IP is indicated the function
  returns it's estimated location, otherwise it will use
  the IP of the system
}
\examples{
# Get computer location
location <- getLocationFromFreegeoip()

# Get '10.10.10.10' location
location <- getLocationFromFreegeoip('10.10.10.10')
}


\name{getExternalIP}
\alias{getExternalIP}
\alias{getExternalIP.whatismyipaddress}
\title{Get the external IP of the computer}
\usage{
  getExternalIP()

  getExternalIP.whatismyipaddress()
}
\description{
  Returns the external IP of the computer if it is
  connected to internet, otherwise returns NULL
}
\examples{
IP <- getExternalIP()
}


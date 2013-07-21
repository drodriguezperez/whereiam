\name{getExternalIP}
\alias{getExternalIP}
\alias{getExternalIP.icanhazip}
\alias{getExternalIP.ifconfig}
\alias{getExternalIP.whatismyipaddress}
\title{Get the external IP of the computer}
\usage{
  getExternalIP()

  getExternalIP.whatismyipaddress()

  getExternalIP.ifconfig()

  getExternalIP.icanhazip()
}
\description{
  Returns the external IP of the computer if it is
  connected to internet, otherwise returns NULL
}
\examples{
IP <- getExternalIP()
}


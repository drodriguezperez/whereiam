\name{getExternalIP}
\alias{getExternalIP}
\alias{getExternalIP.appspot}
\alias{getExternalIP.icanhazip}
\alias{getExternalIP.ifconfig}
\alias{getExternalIP.ipecho}
\alias{getExternalIP.whatismyipaddress}
\title{Get the external IP of the computer}
\usage{
  getExternalIP(service = "whatismyipaddress")

  getExternalIP.whatismyipaddress()

  getExternalIP.ifconfig()

  getExternalIP.icanhazip()

  getExternalIP.ipecho()

  getExternalIP.appspot()
}
\arguments{
  \item{service}{the optional service used to obtain the
  external IP}
}
\description{
  Returns the external IP of the computer if it is
  connected to internet, otherwise returns NULL
}
\examples{
IP <- getExternalIP()
}


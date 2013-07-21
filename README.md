# whereiam

`whereiam` is a geolocation package for `R`.

## Installation

You can install `whereiam` from `github` using the `devtools` package

```
require(devtools)
install_github('whereiam', 'drodriguezperez')
```

## Features

The `whereiam` allow to access to ip based geolocation services from `R`. It is also include on the package the access to different services of geocoding and reverse geocoding.

## Examples

The external IP can be obtained using

```
ip <- getExternalIP()
```

The function returns a string with the IP address, if there is no IP address, for example by not being connected to the Internet, will return `NULL`. From the IP address it can be obtained geographical location information using:

```
loc <- getLocation(ip)
```

Finally it is possible to get the address by using the geographic coordinates using reverse geocoding services with the function:

```
info <- getGeocoding(loc$latitude, loc$longitude)
```
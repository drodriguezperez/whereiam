# whereiam

`whereiam` is a geolocation package for `R`.

## Installation

You can install `whereiam` from `github` using the `devtools` package

```
require(devtools)
install_github('whereiam', 'drodriguezperez')
```

## Features

`whereiam` allow to access to IP based geolocation services from `R`. The package also includes access to different services of geocoding and reverse geocoding.

The coordinates can be stored in objects that allows performing calculations such as distance, the half-way point or the bearing of two points.

### Geocoding examples

The external IP can be obtained using

```
ip = getExternalIP()
```

The function returns a string with the IP address, if there is no IP address, for example by not being connected to the Internet, will return `NULL`. From the IP address it can be obtained geographical location information using:

```
cord <- getIpCoordinates(ip)
```

Finally it is possible to get the address by using the geographic coordinates using reverse geocoding services with the function:

```
info <- reverseGeocoding(cord$latitude, cord$longitude)
```

### Calculation examples

It is possible to create new geographic points through introduce the latitude and longitude in the method `Coordinate`:

```
posc <- Coordinate(42.8806027, -8.5445684)
```

and calculate different values

* Distance in km: `haversineDistance(cord, posc)`
* Half-way poin: `midpoint(cord, posc)`
* Bearing: `bearing(cord, posc)`

## License

[GNU General Public License, version 3](http://www.gnu.org/licenses/gpl-3.0.txt)

##
##  Coordinate S3 class
##
##  Created by Daniel Rodríguez Pérez on 28/7/2013.
##
##  Copyright (c) 2013 Daniel Rodríguez Pérez.
##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>
##

#' Create a Coordinate object
#' 
#' The method create a new Coordinate object type using the indicated
#' latitude and longitude
#' 
#' @param latitude a latitude coordinate
#' @param longitude a longitude coordinate
#' 
#' @rdname Coordinate
#' @export Coordinate
#' @aliases Coordinate
Coordinate <- function(latitude, longitude) {
  latitude  <- validate.Latitude(latitude)
  longitude <- validate.Longitude(longitude)
  
  if (is.latitude(latitude) && is.longitude(longitude)) {
    obj <- list(latitude  = latitude,
                longitude = longitude)
    class(obj) <- 'Coordinate'
    return(obj)
  } else {
    if (!is.latitude(latitude) && !is.longitude(longitude)) {
      stop('The latitude and longitude are not valid value')
    } else if (!is.latitude(latitude)) {
      stop('The latitude is not a valid value')
    } else {
      stop('The longitude is not a valid value')
    }
  }
}

#' Extract latitude from a Coordinate
#' 
#' Get the latitude from a Coordinate or a GreatCircle object in the indicated
#' units (degrees or radians)
#' 
#' @param coordinate a coordinate class
#' @param units a string with the units (degrees or radians)
#' 
#' @rdname getLatitude
#' @export getLatitude
getLatitude <- function(coordinate,
                        units = 'degrees') {
  UseMethod("getLatitude")
}

#' @rdname getLatitude
#' @method getLatitude default
#' @export
getLatitude.default <- function(coordinate,
                                units = 'degrees') {
  result <- deg2any(coordinate, units = units)
  return(result)
}

#' @rdname getLatitude
#' @method getLatitude Coordinate
#' @export
getLatitude.Coordinate <- function(coordinate,
                                   units = 'degrees') {
  result <- getLatitude(coordinate$latitude, units = units)
  return(result)
}

#' Extract longitude from a Coordinate
#' 
#' Get the longitude from a Coordinate or a GreatCircle object in the indicated
#' units (degrees or radians)
#' 
#' @param coordinate a coordinate class
#' @param units a string with the units (degrees or radians)
#' 
#' @rdname getLongitude
#' @export getLongitude
getLongitude <- function(coordinate,
                         units = 'degrees') {
  UseMethod("getLongitude")
}

#' @rdname getLongitude
#' @method getLongitude default
#' @export
getLongitude.default <- function(coordinate,
                                 units = 'degrees') {
  result <- deg2any(coordinate, units = units)
  return(result)
}

#' @rdname getLongitude
#' @method getLongitude Coordinate
#' @export
getLongitude.Coordinate <- function(coordinate,
                                    units = 'degrees') {
  result <- getLongitude(coordinate$longitude, units = units)
  return(result)
}

#' Calculate antipodes
#' 
#' Calculate the antipodes of the idicated point.
#' 
#' @param latitude a latitude coordinate
#' @param longitude a longitude coordinate
#' @param coordinate a coordinate class
#' @param ... other arguments
#' 
#' @rdname antipode
#' @export antipode
antipode <- function(...){
  UseMethod("antipode")
}

#' @rdname antipode
#' @method antipode default
#' @export
antipode.default <- function(latitude, longitude, ...) {
  result <- antipode(Coordinate(latitude, longitude))
  return(result)
}

#' @rdname antipode
#' @method antipode Coordinate
#' @export
antipode.Coordinate <- function(coordinate, ...) {  
  latitude  <- - coordinate$latitude
  longitude <- coordinate$longitude
  
  if (longitude > 0) {
    longitude <- longitude - 180
  } else {
    longitude <- longitude + 180
  }
  
  result <- Coordinate(latitude, longitude)
  return(result)
}
                                        
#' Add distance in the latitude direction to a coordinate
#' 
#' Add distance in the latitude direction to the indicate coordinate. The
#' distance can be indicated on kilometers or miles
#' 
#' @param latitude a latitude coordinate
#' @param longitude a longitude coordinate
#' @param distance the distance to add in km or miles
#' @param units a string with the distance units (default kilometers)
#' @param coordinate a coordinate class
#' @param ... other arguments
#' 
#' @return a Coordinate object with the new coordinates
#' 
#' @examples
#' # Add 10 kilometers to the north to a position
#' cord <- Coordinate(43, -8)
#' cord <- moveLatitude(cord, 10)
#' 
#' @rdname moveLatitude
#' @export moveLatitude
moveLatitude <- function(...){
  UseMethod("moveLatitude")
}

#' @rdname moveLatitude
#' @method moveLatitude default
#' @export
moveLatitude.default <- function(latitude, longitude, distance,
                                 units = 'km', ...) {
  result <-   moveLatitude(Coordinate(latitude, longitude),
                           distance,
                           units = units)
  return(result)
}

#' @rdname moveLatitude
#' @method moveLatitude Coordinate
#' @export
moveLatitude.Coordinate <- function(coordinate, distance,
                                    units = 'km', ...) {
  latitude  <- coordinate$latitude
  longitude <- coordinate$longitude
  
  distance <- units2Kilometers(distance, units)  
  latitude <- latitude + (distance * 360 / (EARTH_DIAMETER_KM * pi))
  result   <- Coordinate(latitude, longitude)
  
  return(result)
}

#' Add distance in the longitude direction to a coordinate
#' 
#' Add distance in the longitude direction to the indicate coordinate. The
#' distance can be indicated on kilometers or miles
#' 
#' @param latitude a latitude coordinate
#' @param longitude a longitude coordinate
#' @param distance the distance to add in km or miles
#' @param units a string with the distance units (default kilometers)
#' @param coordinate a coordinate class
#' @param ... other arguments
#' 
#' @return a Coordinate object with the new coordinates
#' 
#' @examples
#' # Add 10 kilometers to the east to a position
#' cord <- Coordinate(43, -8)
#' cord <- moveLongitude(cord, 10)
#' 
#' @rdname moveLongitude
#' @export moveLongitude
moveLongitude <- function(...){
  UseMethod("moveLongitude")
}

#' @rdname moveLongitude
#' @method moveLongitude default
#' @export
moveLongitude.default <- function(latitude, longitude, distance,
                                         units = 'km', ...) {
  result <- moveLongitude(Coordinate(latitude, longitude),
                          distance,
                          units = units)
  return(result)
}

#' @rdname moveLongitude
#' @method moveLongitude Coordinate
#' @export
moveLongitude.Coordinate <- function(coordinate, distance,
                                             units = 'km', ...) {
  latitude  <- coordinate$latitude
  longitude <- coordinate$longitude
  
  distance  <- units2Kilometers(distance, units)  
  longitude <- longitude +
    (distance * 360 / (EARTH_DIAMETER_KM * pi * cos(deg2rad(latitude))))
  result <- Coordinate(latitude, longitude)
  
  return(result)
}

#' Calculate distance between two points along great circle using the
#' haversine formulae
#' 
#' The function calculates great-circle distances between the two points using
#' the "Haversine" formulae. This is the shortest distance over the earth's
#' surface ignoring the geographic shape like hills and vales.
#' 
#' @param latitude1 the first latitude coordinate
#' @param longitude1 the first longitude coordinate
#' @param latitude2 the second latitude coordinate
#' @param longitude2 the second longitude coordinate
#' @param units a string with the distance units (default kilometers)
#' @param coordinate1 the first coordinate class variable
#' @param coordinate2 the second coordinate class variable
#' @param ... other arguments
#' 
#' @examples
#' cord1    <- Coordinate(43, -8)
#' cord2    <- Coordinate(42, -7)
#' distance <- haversineDistance(cord1, cord2)
#' 
#' @references
#' http://www.movable-type.co.uk/scripts/latlong.html
#' 
#' @rdname haversineDistance
#' @export haversineDistance
haversineDistance <- function(...) {
  UseMethod("haversineDistance")
}

#' @rdname haversineDistance
#' @method haversineDistance default
#' @export
haversineDistance.default <- function(latitude1, longitude1, latitude2, longitude2,
                                      units = 'km', ...) {
  result <- haversineDistance(Coordinate(latitude1, longitude1),
                              Coordinate(latitude2, longitude2),
                              units = units)
  return(result)
}

#' @rdname haversineDistance
#' @method haversineDistance Coordinate
#' @export
haversineDistance.Coordinate <- function(coordinate1, coordinate2,
                                          units = 'km', ...) {
  latitude1  <- getLatitude(coordinate1,  units = 'radians')
  longitude1 <- getLongitude(coordinate1, units = 'radians')
  latitude2  <- getLatitude(coordinate2,  units = 'radians')
  longitude2 <- getLongitude(coordinate2, units = 'radians')
  
  incLatitude  <- latitude2 - latitude1
  incLongitude <- longitude2 - longitude1
  
  a <- sin(incLatitude/2)^2 + sin(incLongitude/2)^2 *
    cos(latitude1) * cos(latitude2)
  
  result <- EARTH_DIAMETER_KM * atan2(sqrt(a), sqrt(1-a))
  result <- kilometers2Units(result, units)  
  
  return(result)
}

#' Calculate distance between two points along great circle using the
#' Spherical Law of Cosines
#' 
#' The function calculates distances between the two points along great circle
#' using the Spherical Law of Cosines formulae. This is the shortest distance
#' over the earth's surface ignoring the geographic shape like hills and vales.
#' 
#' @param latitude1 the first latitude coordinate
#' @param longitude1 the first longitude coordinate
#' @param latitude2 the second latitude coordinate
#' @param longitude2 the second longitude coordinate
#' @param units a string with the distance units (default kilometers)
#' @param coordinate1 the first coordinate class variable
#' @param coordinate2 the second coordinate class variable
#' @param ... other arguments
#' 
#' @examples
#' cord1    <- Coordinate(43, -8)
#' cord2    <- Coordinate(42, -7)
#' distance <- sphericalDistance(cord1, cord2)
#' 
#' @rdname sphericalDistance
#' @export sphericalDistance
sphericalDistance <- function(...) {
  UseMethod("sphericalDistance")
}

#' @rdname sphericalDistance
#' @method sphericalDistance default
#' @export
sphericalDistance.default <- function(latitude1, longitude1, latitude2, longitude2,
                                      units = 'km', ...) {
  result <- sphericalDistance(Coordinate(latitude1, longitude1),
                              Coordinate(latitude2, longitude2),
                              units = units)
  return(result)
}

#' @rdname sphericalDistance
#' @method sphericalDistance Coordinate
#' @export
sphericalDistance.Coordinate <- function(coordinate1, coordinate2,
                                         units = 'km', ...) {
  latitude1  <- getLatitude(coordinate1,  units = 'radians')
  longitude1 <- getLongitude(coordinate1, units = 'radians')
  latitude2  <- getLatitude(coordinate2,  units = 'radians')
  longitude2 <- getLongitude(coordinate2, units = 'radians')
  
  result <- EARTH_DIAMETER_KM * acos(sin(latitude1) * sin(latitude2) +
                                       cos(latitude1) * cos(latitude2) *
                                       cos(longitude2 - longitude1)) / 2
  result <- kilometers2Units(result, units)  
  
  return(result)
}

#' Calculate distance between two points along great circle using the
#' Vincenty's formulae
#' 
#' The function calculates distances between the two points along great circle
#' using the Vincenty's formulae. This is an iterative method which is based
#' on the assumption that the figure of the Earth is an oblate spheroid, and
#' for that reason is more precise than the Spherical Law of Cosines formulae.
#' 
#' @param latitude1 the first latitude coordinate
#' @param longitude1 the first longitude coordinate
#' @param latitude2 the second latitude coordinate
#' @param longitude2 the second longitude coordinate
#' @param units a string with the distance units (default kilometers)
#' @param coordinate1 the first coordinate class variable
#' @param coordinate2 the second coordinate class variable
#' @param maxIter maximum number of iterations in the calculation
#' @param convCrite convergence criteria for the calculation
#' @param ... other arguments
#' 
#' @examples
#' cord1    <- Coordinate(43, -8)
#' cord2    <- Coordinate(42, -7)
#' distance <- vincentyDistance(cord1, cord2)
#' 
#' @references
#' http://www.movable-type.co.uk/scripts/latlong-vincenty.html
#' 
#' @rdname vincentyDistance
#' @export vincentyDistance
vincentyDistance <- function(...) {
  UseMethod("vincentyDistance")
}

#' @rdname vincentyDistance
#' @method vincentyDistance default
#' @export
vincentyDistance.default <- function(latitude1, longitude1, latitude2, longitude2,
                                     units     = 'km',
                                     maxIter   = 100,
                                     convCrite = 1e-12,...) {
  result <- vincentyDistance(Coordinate(latitude1, longitude1),
                             Coordinate(latitude2, longitude2),
                             units     = units,
                             maxIter   = maxIter,
                             convCrite = convCrite)
  return(result)
}

#' @rdname vincentyDistance
#' @method vincentyDistance Coordinate
#' @export
vincentyDistance.Coordinate <- function(coordinate1, coordinate2,
                                        units     = 'km',
                                        maxIter   = 100,
                                        convCrite = 1e-12,...) {
  latitude1  <- getLatitude(coordinate1,  units = 'radians')
  longitude1 <- getLongitude(coordinate1, units = 'radians')
  latitude2  <- getLatitude(coordinate2,  units = 'radians')
  longitude2 <- getLongitude(coordinate2, units = 'radians')
  
  L     <- longitude2 - longitude1
  U1    <- atan((1 - FLATTENING) * tan(latitude1))
  U2    <- atan((1 - FLATTENING) * tan(latitude2)) 
  sinU1 <- sin(U1)
  cosU1 <- cos(U1)
  sinU2 <- sin(U2)
  cosU2 <- cos(U2)
  
  cosSqAlpha <- NULL
  sinSigma   <- NULL
  cosSigma   <- NULL
  cos2SigmaM <- NULL
  sigma      <- NULL
  
  lambda  <- L
  lambdaP <- 0
  iter    <- 0
  
  while (abs(lambda - lambdaP) > convCrite & iter < maxIter) {
    sinLambda <- sin(lambda)
    cosLambda <- cos(lambda)
    sinSigma <- sqrt((cosU2 * sinLambda) * (cosU2 * sinLambda) +
                       (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda) *
                       (cosU1 * sinU2 - sinU1 * cosU2 * cosLambda))
    
    if (sinSigma == 0) {
      return(0)
    }
    
    cosSigma   <- sinU1 * sinU2 + cosU1 * cosU2 * cosLambda
    sigma      <- atan2(sinSigma, cosSigma)
    sinAlpha   <- cosU1 * cosU2 * sinLambda / sinSigma
    cosSqAlpha <- 1 - sinAlpha * sinAlpha
    cos2SigmaM <- cosSigma - 2 * sinU1 * sinU2 / cosSqAlpha
    
    if (is.na(cos2SigmaM)) {
      cos2SigmaM <- 0
    }
    
    C       <- FLATTENING / 16 * cosSqAlpha *
      (4 + FLATTENING * (4 - 3 * cosSqAlpha))
    lambdaP <- lambda
    lambda  <- L + (1 - C) * FLATTENING * sinAlpha *
      (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
    
    iter <- iter + 1
  }
  
  if (iter >= maxIter) {
    return(NA)
  }
  
  uSq        <- cosSqAlpha *
    (SEMI_MAJOR_AXIS * SEMI_MAJOR_AXIS -  SEMI_MINOR_AXIS * SEMI_MINOR_AXIS) /
    (SEMI_MINOR_AXIS * SEMI_MINOR_AXIS)
  A          <- 1 + uSq / 16384 * (4096 + uSq * (-768 + uSq * (320 - 175 * uSq)))
  B          <- uSq / 1024 * (256 + uSq *(-128 + uSq * (74 - 47 * uSq)))
  deltaSigma <- B * sinSigma * (cos2SigmaM + B / 4 *
                                  (cosSigma * (-1 + 2 * cos2SigmaM^2) -
                                     B / 6 * cos2SigmaM * 
                                     (- 3 + 4 * sinSigma^2) * (-3 + 4 * cos2SigmaM^2)))
  
  result <- SEMI_MINOR_AXIS * A * (sigma - deltaSigma) / 1000
  result <- kilometers2Units(result, units)  
  
  return(result)
}

#' Calculate distance between two points along a rhumb line
#' 
#' The function calculates distances between the two points along a rhumb
#' line. The rhumb lines are path of constant bearing and crosses all
#' meridians at the same angle. The rhumb lines are generally longer than
#' great-circle routes.
#' 
#' @param latitude1 the first latitude coordinate
#' @param longitude1 the first longitude coordinate
#' @param latitude2 the second latitude coordinate
#' @param longitude2 the second longitude coordinate
#' @param units a string with the distance units (default kilometers)
#' @param coordinate1 the first coordinate class variable
#' @param coordinate2 the second coordinate class variable
#' @param ... other arguments
#' 
#' @examples
#' cord1    <- Coordinate(43, -8)
#' cord2    <- Coordinate(42, -7)
#' distance <- rhumbDistance(cord1, cord2)
#' 
#' @references
#' http://www.movable-type.co.uk/scripts/latlong.html
#' 
#' @rdname rhumbDistance
#' @export rhumbDistance
rhumbDistance <- function(...) {
  UseMethod("rhumbDistance")
}

#' @rdname rhumbDistance
#' @method rhumbDistance default
#' @export
rhumbDistance.default <- function(latitude1, longitude1, latitude2, longitude2,
                                  units = 'km', ...) {
  result <- vincentyDistance(Coordinate(latitude1, longitude1),
                             Coordinate(latitude2, longitude2),
                             units = units)
  return(result)
}

#' @rdname rhumbDistance
#' @method rhumbDistance Coordinate
#' @export
rhumbDistance.Coordinate <- function(coordinate1, coordinate2,
                                     units = 'km', ...) {
  latitude1  <- getLatitude(coordinate1,  units = 'radians')
  longitude1 <- getLongitude(coordinate1, units = 'radians')
  latitude2  <- getLatitude(coordinate2,  units = 'radians')
  longitude2 <- getLongitude(coordinate2, units = 'radians')
  
  dLatitude  <- latitude2 - latitude1
  dLongitude <- longitude2 - longitude1
  dPhi       <- log(tan(pi / 4 + latitude2 / 2) / tan(pi / 4 + latitude1 / 2))
  
  if (dPhi != 0) {
    q <- dLatitude / dPhi
  } else {
    q <- cos(latitude1)
  }
  
  if (abs(dLongitude) > pi) {
    if (dLongitude > 0) {
      dLon <- - 2 * pi - dLongitude
    } else {
      dLon <- 2 * pi + dLongitude
    }
  }
  
  result <- sqrt(dLatitude^2 + q * q * dLongitude^2) * EARTH_DIAMETER_KM / 2
  result <- kilometers2Units(result, units)  
  
  return(result)
}

#' Initial bearing between two points
#'  
#' Calculate the initial bearing between two points. This bearing which if
#' followed in a straight line along a great-circle arc will take you from
#' the start point to the end point. The point can be calculated along a
#' great-circle or a rhumb line.
#' 
#' @param latitude1 the first latitude coordinate
#' @param longitude1 the first longitude coordinate
#' @param latitude2 the second latitude coordinate
#' @param longitude2 the second longitude coordinate
#' @param coordinate1 the first coordinate class variable
#' @param coordinate2 the second coordinate class variable
#' @param line a string with the line used to calculate greatcircle or rhumb
#' @param ... other arguments
#' 
#' @examples
#' cord1 <- Coordinate(43, -8)
#' cord2 <- Coordinate(42, -7)
#' 
#' # Using great-circle
#' brg   <- bearing(cord1, cord2)
#' 
#' # Using rhumb
#' brg   <- bearing(cord1, cord2, line = 'rhumb')
#' 
#' @references
#' http://www.movable-type.co.uk/scripts/latlong.html
#' 
#' @rdname bearing
#' @export bearing
bearing <- function(...) {
  UseMethod("bearing")
}

#' @rdname bearing
#' @method bearing default
#' @export
bearing.default <- function(latitude1, longitude1, latitude2, longitude2,
                            line = 'greatcircle', ...) {
  result <- bearing(Coordinate(latitude1, longitude1),
                    Coordinate(latitude2, longitude2),
                    line = line)
  return(result)
}

#' @rdname bearing
#' @method bearing Coordinate
#' @export
bearing.Coordinate <- function(coordinate1, coordinate2,
                               line = 'greatcircle', ...) {
  line       <- tolower(line)
  latitude1  <- getLatitude(coordinate1,  units = 'radians')
  longitude1 <- getLongitude(coordinate1, units = 'radians')
  latitude2  <- getLatitude(coordinate2,  units = 'radians')
  longitude2 <- getLongitude(coordinate2, units = 'radians')
  result     <- NULL
  
  if (line == 'greatcircle') {
    y  <- sin(longitude2 - longitude1) * cos(latitude2)
    x1 <- cos(latitude1) * sin(latitude2)
    x2 <- sin(latitude1) * cos(latitude2) * cos(longitude2 - longitude1)
    
    result <- rad2deg(atan2(y, x1 - x2))
    
  } else if (line == 'rhumb') {
    dLatitude  <- latitude2 - latitude1
    dLongitude <- longitude2 - longitude1
    dPhi       <- log(tan(pi/4 + latitude2/2) / tan(pi/4 + latitude1/2))
    
    if (dPhi != 0) {
      q <- dLatitude / dPhi
    } else {
      q <- cos(latitude1)
    }
    
    if (abs(dLongitude) > pi) {
      if (dLongitude > 0) {
        dLon <- - 2 * pi - dLongitude
      } else {
        dLon <- 2 * pi + dLongitude
      }
    }
    
    result <- rad2deg(atan2(dLongitude, dPhi))
    
  } else {
    stop(sprintf('the line %s is not supported', line))
  }

  return(result)
}

#' Midpoint between two points
#'  
#' Calculate the half-way point along a great circle path between the two
#' points. The point can be calculated along a great-circle or a rhumb line.
#' 
#' @param latitude1 the first latitude coordinate
#' @param longitude1 the first longitude coordinate
#' @param latitude2 the second latitude coordinate
#' @param longitude2 the second longitude coordinate
#' @param coordinate1 the first coordinate class variable
#' @param coordinate2 the second coordinate class variable
#' @param line a string with the line used to calculate greatcircle or rhumb
#' @param ... other arguments
#' 
#' @examples
#' cord1 <- Coordinate(43, -8)
#' cord2 <- Coordinate(42, -7)
#' 
#' # Using great-circle
#' mcord <- midpoint(cord1, cord2)
#' 
#' # Using rhumb
#' mcord <- midpoint(cord1, cord2, line = 'rhumb')
#' 
#' @references
#' http://www.movable-type.co.uk/scripts/latlong.html
#' 
#' @rdname midpoint
#' @export midpoint
midpoint <- function(...) {
  UseMethod("midpoint")
}

#' @rdname midpoint
#' @method midpoint default
#' @export
midpoint.default <- function(latitude1, longitude1, latitude2, longitude2,
                             line = 'greatcircle', ...) {
  result <- midpoint(Coordinate(latitude1, longitude1),
                     Coordinate(latitude2, longitude2),
                     line = line)
  return(result)
}

#' @rdname midpoint
#' @method midpoint Coordinate
#' @export
midpoint.Coordinate <- function(coordinate1, coordinate2,
                                line = 'greatcircle', ...) {
  line       <- tolower(line)
  latitude1  <- getLatitude(coordinate1,  units = 'radians')
  longitude1 <- getLongitude(coordinate1, units = 'radians')
  latitude2  <- getLatitude(coordinate2,  units = 'radians')
  longitude2 <- getLongitude(coordinate2, units = 'radians')
  result     <- NULL
  
  if (line == 'greatcircle') {
    x <- cos(latitude2) * cos(longitude2 - longitude1)
    y <- cos(latitude2) * sin(longitude2 - longitude1)
    
    latitude  <- atan2(sin(latitude1) + sin(latitude2), sqrt((cos(latitude1) + x)^2 +y^2))
    longitude <- longitude1 + atan2(y, cos(latitude1) + x)
    
    result <- Coordinate(rad2deg(latitude), rad2deg(longitude))
    
  } else if (line == 'rhumb') {
    latitude  <- (latitude1 + latitude2) / 2
    longitude <- ((longitude2 - longitude1)  * log(tan(pi /4 + latitude / 2)) +
                    longitude1 * log(tan(pi/4 + latitude2 / 2)) -
                    longitude2 * log(tan(pi / 4 + latitude1 / 2))) / 
      log(tan(pi/4 + latitude2 / 2) / tan(pi / 4 + latitude1 / 2))

    if (!is.finite(longitude)) {
      longitude <- (longitude1 + longitude2) / 2
    }
    
    longitude = (longitude + 3 * pi) %% (2 * pi) - pi
    
    result <- Coordinate(rad2deg(latitude), rad2deg(longitude))
    
  } else {
    stop(sprintf('the line %s is not supported', line))
  }
  
  return(result)
}

#' Destination point from start, bearing and distance along great circle
#' 
#' Obtain a destination point from a given start point, initial bearing,
#' and distance
#' 
#' @param latitude a latitude coordinate
#' @param longitude a longitude coordinate
#' @param brg the initial bearing
#' @param distance the distance to add in km or miles
#' @param units a string with the distance units (default kilometers)
#' @param coordinate a coordinate class
#' @param ... other argument
#' 
#' @examples
#' # Add 10 kilometers to icord
#' icord <- Coordinate(43, -8)
#' ecord <- destination(icord, 30, 10)
#' 
#' @references
#' http://www.movable-type.co.uk/scripts/latlong.html
#' 
#' @rdname destination
#' @export destination
destination <- function(...) {
  UseMethod("destination")
}

#' @rdname destination
#' @method destination default
#' @export
destination.default <- function(latitude, longitude, brg, distance,
                                units = 'km', ...) {
  result <- destination(Coordinate(latitude, longitude), brg, distance,
                        units = units)
  return(result)
}

#' @rdname destination
#' @method destination Coordinate
#' @export
destination.Coordinate <- function(coordinate, brg,
                                    distance, units = 'km', ...) {
  latitude  <- getLatitude(coordinate,  units = 'radians')
  longitude <- getLongitude(coordinate, units = 'radians')
  
  distance        <- units2Kilometers(distance, units)    
  angularDistance <- 2 * distance / EARTH_DIAMETER_KM
  brg             <- deg2rad(brg)
  
  newLatitude  <- asin(sin(latitude) * cos(angularDistance) + 
                         cos(latitude) * sin(angularDistance) * cos(brg))
  newLongitude <- longitude + atan2(sin(brg) * sin(angularDistance) * cos(longitude),
                                    cos(angularDistance) -
                                      sin(latitude) * sin(newLatitude))
  
  result <- Coordinate(rad2deg(newLatitude), rad2deg(newLongitude))
  
  return(result)
}

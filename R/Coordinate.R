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
#' cord <- addDistanceLatitude(cord, 10)
#' 
#' @rdname addDistanceLatitude
#' @export addDistanceLatitude
addDistanceLatitude <- function(...){
  UseMethod("addDistanceLatitude")
}

#' @rdname addDistanceLatitude
#' @method addDistanceLatitude default
#' @S3method addDistanceLatitude default
addDistanceLatitude.default <- function(latitude, longitude, distance, units = 'km', ...) {
  distance <- units2Kilometers(distance, units)  
  latitude <- latitude + (distance * 360 / (EARTH_DIAMETER_KM * pi))
  result   <- Coordinate(latitude, longitude)
  
  return(result)
}

#' @rdname addDistanceLatitude
#' @method addDistanceLatitude Coordinate
#' @S3method addDistanceLatitude Coordinate
addDistanceLatitude.Coordinate <- function(coordinate, distance,
                                            units = 'km', ...) {
  addDistanceLatitude(coordinate$latitude,
                      coordinate$longitude,
                      distance,
                      units = units)
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
#' cord <- addDistanceLongitude(cord, 10)
#' 
#' @rdname addDistanceLongitude
#' @export addDistanceLongitude
addDistanceLongitude <- function(...){
  UseMethod("addDistanceLongitude")
}

#' @rdname addDistanceLongitude
#' @method addDistanceLongitude default
#' @S3method addDistanceLongitude default
addDistanceLongitude.default <- function(latitude, longitude, distance,
                                         units = 'km', ...) {
  distance  <- units2Kilometers(distance, units)  
  longitude <- longitude +
    (distance * 360 / (EARTH_DIAMETER_KM * pi * cos(deg2rad(latitude))))
  result <- Coordinate(latitude, longitude)
  
  return(result)
}

#' @rdname addDistanceLongitude
#' @method addDistanceLongitude Coordinate
#' @S3method addDistanceLongitude Coordinate
addDistanceLongitude.Coordinate <- function(coordinate, distance,
                                             units = 'km', ...) {
  addDistanceLongitude(coordinate$latitude,
                       coordinate$longitude,
                       distance,
                       units = units)
}

#' Calculate distance between two points
#' 
#' The function calculates great-circle distances between the two points using
#' the "Haversine" formula. This is the shortest distance over the earth's
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
#' @rdname haversineDistance
#' @export haversineDistance
haversineDistance <- function(...) {
  UseMethod("haversineDistance")
}

#' @rdname haversineDistance
#' @method haversineDistance default
#' @S3method haversineDistance default
haversineDistance.default <- function(latitude1, longitude1,
                                      latitude2, longitude2,
                                      units = 'km', ...) {
  incLatitude  <- deg2rad(latitude2 - latitude1)
  incLongitude <- deg2rad(longitude2 - longitude1)
  
  a <- sin(incLatitude/2)^2 + sin(incLongitude/2)^2 *
    cos(deg2rad(latitude1)) * cos(deg2rad(latitude2))
  
  result <- EARTH_DIAMETER_KM * atan2(sqrt(a), sqrt(1-a))
  result <- kilometers2Units(result, units)  
  
  return(result)
}

#' @rdname haversineDistance
#' @method haversineDistance Coordinate
#' @S3method haversineDistance Coordinate
haversineDistance.Coordinate <- function(coordinate1, coordinate2,
                                          units = 'km', ...) {
  haversineDistance(coordinate1$latitude,
                    coordinate1$longitude,
                    coordinate2$latitude,
                    coordinate2$longitude,
                    units = units)
}

#' Calculate distance between two points
#' 
#' The function calculates distances between the two points using the
#' Spherical Law of Cosines formula.
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
#' @S3method sphericalDistance default
sphericalDistance.default <- function(latitude1, longitude1,
                                      latitude2, longitude2,
                                      units = 'km', ...) {
  latitude1  <- deg2rad(latitude1)
  longitude1 <- deg2rad(longitude1)
  latitude2  <- deg2rad(latitude2)
  longitude2 <- deg2rad(longitude2)
  
  result <- EARTH_DIAMETER_KM * acos(sin(latitude1) * sin(latitude2) +
                                       cos(latitude1) * cos(latitude2) *
                                       cos(longitude2 - longitude1))
  result <- kilometers2Units(result, units)  
  
  return(result)
}

#' @rdname sphericalDistance
#' @method sphericalDistance Coordinate
#' @S3method sphericalDistance Coordinate
sphericalDistance.Coordinate <- function(coordinate1, coordinate2,
                                         units = 'km', ...) {
  sphericalDistance(coordinate1$latitude,
                    coordinate1$longitude,
                    coordinate2$latitude,
                    coordinate2$longitude,
                    units = units)
}

#' Initial bearing between two points
#'  
#' Calculate the initial bearing between two points. This bearing which if
#' followed in a straight line along a great-circle arc will take you from
#' the start point to the end point
#' 
#' @param latitude1 the first latitude coordinate
#' @param longitude1 the first longitude coordinate
#' @param latitude2 the second latitude coordinate
#' @param longitude2 the second longitude coordinate
#' @param coordinate1 the first coordinate class variable
#' @param coordinate2 the second coordinate class variable
#' @param ... other arguments
#' 
#' @examples
#' cord1 <- Coordinate(43, -8)
#' cord2 <- Coordinate(42, -7)
#' brg   <- bearing(cord1, cord2)
#' 
#' @rdname bearing
#' @export bearing
bearing <- function(...) {
  UseMethod("bearing")
}

#' @rdname bearing
#' @method bearing default
#' @S3method bearing default
bearing.default <- function(latitude1, longitude1,
                            latitude2, longitude2, ...) {
  latitude1  <- deg2rad(latitude1)
  longitude1 <- deg2rad(longitude1)
  latitude2  <- deg2rad(latitude2)
  longitude2 <- deg2rad(longitude2)
  
  y  <- sin(longitude2 - longitude1) * cos(latitude2)
  x1 <- cos(latitude1) * sin(latitude2)
  x2 <- sin(latitude1) * cos(latitude2) * cos(longitude2 - longitude1)
  
  result <- rad2deg(atan2(y, x1 - x2))
  
  return(result)
}

#' @rdname bearing
#' @method bearing Coordinate
#' @S3method bearing Coordinate
bearing.Coordinate <- function(coordinate1, coordinate2, ...) {
  bearing(coordinate1$latitude,
          coordinate1$longitude,
          coordinate2$latitude,
          coordinate2$longitude)
}

#' Midpoint between two points
#'  
#' Calculate the half-way point along a great circle path between the two
#' points
#' 
#' @param latitude1 the first latitude coordinate
#' @param longitude1 the first longitude coordinate
#' @param latitude2 the second latitude coordinate
#' @param longitude2 the second longitude coordinate
#' @param coordinate1 the first coordinate class variable
#' @param coordinate2 the second coordinate class variable
#' @param ... other arguments
#' 
#' @examples
#' cord1 <- Coordinate(43, -8)
#' cord2 <- Coordinate(42, -7)
#' mcord <- midpoint(cord1, cord2)
#' 
#' @rdname midpoint
#' @export midpoint
midpoint <- function(...) {
  UseMethod("midpoint")
}

#' @rdname midpoint
#' @method midpoint default
#' @S3method midpoint default
midpoint.default <- function(latitude1, longitude1,
                             latitude2, longitude2, ...) {
  latitude1  <- deg2rad(latitude1)
  longitude1 <- deg2rad(longitude1)
  latitude2  <- deg2rad(latitude2)
  longitude2 <- deg2rad(longitude2)
  
  x <- cos(latitude2) * cos(longitude2 - longitude1)
  y <- cos(latitude2) * sin(longitude2 - longitude1)
  
  latitude  <- atan2(sin(latitude1) + sin(latitude2), sqrt((cos(latitude1) + x)^2 +y^2))
  longitude <- longitude1 + atan2(y, cos(latitude1) + x)
  
  result <- Coordinate(rad2deg(latitude), rad2deg(longitude))
  
  return(result)
}

#' @rdname midpoint
#' @method midpoint Coordinate
#' @S3method midpoint Coordinate
midpoint.Coordinate <- function(coordinate1, coordinate2, ...) {
  midpoint(coordinate1$latitude,
           coordinate1$longitude,
           coordinate2$latitude,
           coordinate2$longitude)
}

#' Destination point from start, bearing and distance
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
#' @rdname destination
#' @export destination
destination <- function(...) {
  UseMethod("destination")
}

#' @rdname destination
#' @method destination default
#' @S3method destination default
destination.default <- function(latitude, longitude, brg,
                                distance, units = 'km', ...) {
  distance        <- units2Kilometers(distance, units)    
  angularDistance <- 2 * distance / EARTH_DIAMETER_KM
  latitude        <- deg2rad(latitude)
  longitude       <- deg2rad(longitude)
  brg             <- deg2rad(brg)
  
  newLatitude  <- asin(sin(latitude) * cos(angularDistance) + 
                         cos(latitude) * sin(angularDistance) * cos(brg))
  newLongitude <- longitude + atan2(sin(brg) * sin(angularDistance) * cos(longitude),
                                    cos(angularDistance) -
                                      sin(latitude) * sin(newLatitude))
  
  result <- Coordinate(rad2deg(newLatitude), rad2deg(newLongitude))
  
  return(result)
}

#' @rdname destination
#' @method destination Coordinate
#' @S3method destination Coordinate
destination.Coordinate <- function(coordinate, brg,
                                    distance, units = 'km', ...) {
  destination(coordinate$latitude,
              coordinate$longitude,
              brg,
              distance,
              units = units)
}

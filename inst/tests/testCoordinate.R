##
##  Unit testing for Coordinate S3 class
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

context("Coordinate S3 class")

MAXERROR <- 1e-6

test_that("is.latitude and is.longitude validation", {
  expect_that(is.latitude(   0), is_true())
  expect_that(is.latitude(  30), is_true())
  expect_that(is.latitude( -30), is_true())
  expect_that(is.latitude(  90), is_true())
  expect_that(is.latitude( -90), is_true())
  expect_that(is.latitude( 120), is_false())
  expect_that(is.latitude(-120), is_false())
  expect_that(is.latitude('10'), is_false())
  
  expect_that(is.longitude(   0), is_true())
  expect_that(is.longitude(  90), is_true())
  expect_that(is.longitude( -90), is_true())
  expect_that(is.longitude( 180), is_true())
  expect_that(is.longitude(-180), is_true())
  expect_that(is.longitude( 230), is_false())
  expect_that(is.longitude(-230), is_false())
  expect_that(is.longitude('10'), is_false())
})

test_that("Coordinate S3 class errors", {
  expect_that(Coordinate(120, 100),
              throws_error('The latitude is not a valid value'))
  expect_that(Coordinate( 90, 300),
              throws_error('The longitude is not a valid value'))
  expect_that(Coordinate(120, 300),
              throws_error('The latitude and longitude are not valid value'))
})

test_that("Haversine distance calculation", {
  test_that(haversineDistance( 0,  0,  0,  0),
            equals(0, tolerance = MAXERROR))
  test_that(haversineDistance( 0,  0, 90, 90),
            equals(10018.75, tolerance = MAXERROR))
  test_that(haversineDistance(40, 30, 45, 25),
            equals(  691.2137, tolerance = MAXERROR))
  test_that(haversineDistance(23, 33, 23, 31),
            equals(  204.9387, tolerance = MAXERROR))
})

test_that("Spherical law of cosines distance calculation", {
  test_that(sphericalDistance( 0,  0,  0,  0),
            equals(0, tolerance = MAXERROR))
  test_that(sphericalDistance( 0,  0, 90, 90),
            equals(10018.75, tolerance = MAXERROR))
  test_that(sphericalDistance(40, 30, 45, 25),
            equals(  691.2137, tolerance = MAXERROR))
  test_that(sphericalDistance(23, 33, 23, 31),
            equals(  204.9387, tolerance = MAXERROR))
})

test_that("Add distance to coordinates", {
  cord0 <- Coordinate(33, 86)
  cord1 <- Coordinate(36, 86)
  
  expect_that(addDistanceLatitude(cord0, 333.9585),
              equals(cord1, tolerance = 33 * MAXERROR))
  
  expect_that(addDistanceLongitude(33, 46, 373.419),
              equals(Coordinate(33, 50), tolerance = 46 * MAXERROR))           
})

test_that("Bearing calculation", {
  expect_that(bearing(90,   0, 90,  0),
              equals(  0, tolerance = MAXERROR))
  expect_that(bearing(90,  10, 90,  0),
              equals(-85, tolerance = MAXERROR))
  expect_that(bearing(90,   0, 90, 10),
              equals( 85, tolerance = MAXERROR))
  expect_that(bearing(90, -10, 90, 10),
              equals( 80, tolerance = MAXERROR))
})

test_that("Bearing calculation", {
  expect_that(midpoint(90,   0, 90,  0),
              equals(Coordinate(90, 0), tolerance = MAXERROR))
  expect_that(midpoint(90,  10, 90,  0),
              equals(Coordinate(90, 5), tolerance = MAXERROR))
  expect_that(midpoint(90,   0, 90, 10),
              equals(Coordinate(90, 5), tolerance = MAXERROR))
  expect_that(midpoint(90, -10, 90, 10),
              equals(Coordinate(90, 0), tolerance = MAXERROR))
  expect_that(midpoint(45,   0, 90,  0),
              equals(Coordinate(67.5, 0), tolerance = MAXERROR))
  expect_that(midpoint(45,  45, 90,  0),
              equals(Coordinate(67.5, 45), tolerance = MAXERROR))
})

test_that("Destination calculation", {
  expect_that(destination(0, 0,  0, 100),
              equals(Coordinate(0.8983153, 0), tolerance = MAXERROR))
  expect_that(destination(0, 0, 45, 100),
              equals(Coordinate(0.6351919, 0.6352309), tolerance = MAXERROR))
  expect_that(destination(0, 0, 90, 100),
              equals(Coordinate(0, 0.8983153), tolerance = MAXERROR))
})

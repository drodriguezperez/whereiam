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
  expect_that(Coordinate('120', 100),
              throws_error('The latitude is not a valid value'))
  expect_that(Coordinate( 90, '300'),
              throws_error('The longitude is not a valid value'))
  expect_that(Coordinate('120', '300'),
              throws_error('The latitude and longitude are not valid value'))
})

test_that("Coordinate S3 class getters", {
  cord <- Coordinate(33, 44)
  
  expect_that(getLatitude(cord), equals(33))
  expect_that(getLatitude(cord, units = 'radians'), equals(deg2rad(33)))
  
  expect_that(getLongitude(cord), equals(44))
  expect_that(getLongitude(cord, units = 'radians'), equals(deg2rad(44)))
})

test_that("Haversine distance calculation", {
  test_that(haversineDistance( 0,  0,  0,  0),
            equals(0, tolerance = MAXERROR))
  test_that(haversineDistance( 0,  0, 90, 90) / 10018.75,
            equals(1, tolerance = MAXERROR))
  test_that(haversineDistance(40, 30, 45, 25) / 691.2137,
            equals(1, tolerance = MAXERROR))
  test_that(haversineDistance(23, 33, 23, 31) / 204.9387,
            equals(1, tolerance = MAXERROR))
})

test_that("Spherical law of cosines distance calculation", {
  test_that(sphericalDistance( 0,  0,  0,  0),
            equals(0, tolerance = MAXERROR))
  test_that(sphericalDistance( 0,  0, 90, 90) / 10018.75,
            equals(1, tolerance = MAXERROR))
  test_that(sphericalDistance(40, 30, 45, 25) / 691.2137,
            equals(1, tolerance = MAXERROR))
  test_that(sphericalDistance(23, 33, 23, 31) / 204.9387,
            equals(1, tolerance = MAXERROR))
})

test_that("Add distance to coordinates", {
  cord0 <- Coordinate(33, 86)
  cord1 <- Coordinate(36, 86)
  
  expect_that(moveLatitude(cord0, 333.9585),
              equals(cord1, tolerance = 33 * MAXERROR))
  
  expect_that(moveLongitude(33, 46, 373.419),
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

test_that("Validate distance methods", {
  # In this test it is used the data from
  # Data from http://codeblow.com/questions/formulas-to-calculate-geo-closeness/
  
  googleHQ    <- Coordinate( 37.422045, -122.084347)
  bayArea     <- Coordinate( 37.77493,  -122.419416)
  eiffelTower <- Coordinate( 48.8582,      2.294407)
  operaHouse  <- Coordinate(-33.856553,  151.214696)
  
  ratio <- EARTH_DIAMETER_KM / (2 * 6371)
  
  expect_that(vincentyDistance(googleHQ, bayArea) / 49.087066,
              equals(1, tolerance = MAXERROR))
  expect_that(haversineDistance(googleHQ, bayArea) / (49.103006 * ratio),
              equals(1, tolerance = MAXERROR))
  expect_that(sphericalDistance(googleHQ, bayArea)  / (49.103006 * ratio),
              equals(1, tolerance = MAXERROR))
  
  expect_that(vincentyDistance(googleHQ, eiffelTower) / 8989.724399,
              equals(1, tolerance = MAXERROR))
  expect_that(haversineDistance(googleHQ, eiffelTower) / (8967.042917 * ratio),
              equals(1, tolerance = MAXERROR))
  expect_that(sphericalDistance(googleHQ, eiffelTower)  / (8967.042917 * ratio),
              equals(1, tolerance = MAXERROR))
  
  expect_that(vincentyDistance(googleHQ, operaHouse) / 11939.773640,
              equals(1, tolerance = MAXERROR))
  expect_that(haversineDistance(googleHQ, operaHouse) / (11952.717240 * ratio),
              equals(1, tolerance = MAXERROR))
  expect_that(sphericalDistance(googleHQ, operaHouse)  / (11952.717240 * ratio),
              equals(1, tolerance = MAXERROR))  
})

test_that("Rhumb lines calculations methods", {
  cord1 <- Coordinate(50, 45)
  cord2 <- Coordinate(45, 40)
  
  ratio <- EARTH_DIAMETER_KM / (2 * 6371)
  
  expect_that(rhumbDistance(cord1, cord2),
              equals(671.4886, tolerance = MAXERROR))
  expect_that(bearing(cord1, cord2, line='rhumb'),
              equals(-145.986, tolerance = MAXERROR))
  expect_that(midpoint(cord1, cord2, line='rhumb'),
              equals(Coordinate(47.5, 42.44043), tolerance = MAXERROR))
  
})

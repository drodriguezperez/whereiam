##
##  Unit testing for Coordinates S3 class
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

context("Coordinates S3 class")

MAXERROR <- 1e-6

test_that("conversions of angles", {
  expect_that(deg2rad( 0), equals(0, tolerance = MAXERROR))
  expect_that(deg2rad(10), equals(0.1745329, tolerance = MAXERROR))
  expect_that(deg2rad(40), equals(0.6981317, tolerance = MAXERROR))
  
  expect_that(dms2deg( 0,  0,  0), equals(0, tolerance = MAXERROR))
  expect_that(dms2deg(10, 30,  0), equals(10.5, tolerance = MAXERROR))
  expect_that(dms2deg(25, 30, 30), equals(25.50833, tolerance = MAXERROR))
  
  expect_that(dms2rad( 0,  0,  0), equals(0, tolerance = MAXERROR))
  expect_that(dms2rad(10, 30,  0), equals(0.1832596, tolerance = MAXERROR))
  expect_that(dms2rad(25, 30, 30), equals(0.4452043, tolerance = MAXERROR))
})

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

test_that("Coordinates S3 class errors", {
  expect_that(Coordinates(120, 100),
              throws_error('The latitude is not a valid value'))
  expect_that(Coordinates( 90, 300),
              throws_error('The longitude is not a valid value'))
  expect_that(Coordinates(120, 300),
              throws_error('The latitude and longitude are not valid value'))
})

test_that("Distance calculation", {
  test_that(getDistance( 0,  0,  0,  0),
            equals(0, tolerance = MAXERROR))
  test_that(getDistance( 0,  0, 90, 90),
            equals(10018.75, tolerance = MAXERROR))
  test_that(getDistance(40, 30, 45, 25),
            equals(  691.2137, tolerance = MAXERROR))
  test_that(getDistance(23, 33, 23, 31),
            equals(  204.9387, tolerance = MAXERROR))
})

test_that("Add distance to coordinates", {
  cord0 <- Coordinates(33, 86)
  cord1 <- Coordinates(36, 86)
  
  expect_that(addDistanceLatitude(cord0, 333.9585),
              equals(cord1, tolerance = 33 * MAXERROR))
  
  expect_that(addDistanceLongitude(33, 46, 373.419),
              equals(Coordinates(33, 50), tolerance = 46 * MAXERROR))           
})

##
##  Test GrearCircle class
##
##  Created by Daniel Rodríguez Pérez on 1/8/2013.
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

context("GrearCircle S3 class")

MAXERROR <- 1e-6

test_that("GrearCircle S3 class getters", {
  grearCircle <- GreatCircle(33, 44, 50)
  
  expect_that(getLatitude(grearCircle), equals(33))
  expect_that(getLatitude(grearCircle, units = 'radians'), equals(deg2rad(33)))
  
  expect_that(getLongitude(grearCircle), equals(44))
  expect_that(getLongitude(grearCircle, units = 'radians'), equals(deg2rad(44)))
  
  expect_that(getBearing(grearCircle), equals(50))
  expect_that(getBearing(grearCircle, units = 'radians'), equals(deg2rad(50)))
  
  grearCircle <- GreatCircle(Coordinate(10, 20), 30)
  
  expect_that(getLatitude(grearCircle), equals(10))
  expect_that(getLatitude(grearCircle, units = 'radians'), equals(deg2rad(10)))
  
  expect_that(getLongitude(grearCircle), equals(20))
  expect_that(getLongitude(grearCircle, units = 'radians'), equals(deg2rad(20)))
  
  expect_that(getBearing(grearCircle), equals(30))
  expect_that(getBearing(grearCircle, units = 'radians'), equals(deg2rad(30)))
})

test_that("GrearCircle intersection", {
  grearCircle1 <- GreatCircle(50, 50, 10)
  grearCircle2 <- GreatCircle(40, 40, 30)
  pointResult  <- Coordinate(dms2deg(50, 49, 43), dms2deg(50, 13, 53))
  
  expect_that(intersection(grearCircle1, grearCircle2),
              equals(pointResult, tolerance = 50 * MAXERROR))
})

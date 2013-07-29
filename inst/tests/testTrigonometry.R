##
##  Tests for trigonometry functions
##
##  Created by Daniel Rodríguez Pérez on 29/7/2013.
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

context("Trigonometry")

MAXERROR <- 1e-6

test_that("degrees to radians", {
  expect_that(deg2rad( 0), equals(0, tolerance = MAXERROR))
  expect_that(deg2rad(10), equals(0.1745329, tolerance = MAXERROR))
  expect_that(deg2rad(40), equals(0.6981317, tolerance = MAXERROR))
})

test_that("degrees minutes and seconds", {
  expect_that(dms2deg( 0,  0,  0), equals(0, tolerance = MAXERROR))
  expect_that(dms2deg(10, 30,  0), equals(10.5, tolerance = MAXERROR))
  expect_that(dms2deg(25, 30, 30), equals(25.50833, tolerance = MAXERROR))
  
  expect_that(dms2rad( 0,  0,  0), equals(0, tolerance = MAXERROR))
  expect_that(dms2rad(10, 30,  0), equals(0.1832596, tolerance = MAXERROR))
  expect_that(dms2rad(25, 30, 30), equals(0.4452043, tolerance = MAXERROR))
})

test_that("radians  to degrees", {
  expect_that(rad2deg( 0   ), equals(0, tolerance = MAXERROR))
  expect_that(rad2deg( 0.5 ), equals(28.64789, tolerance = MAXERROR))
  expect_that(rad2deg( 1.0 ), equals(57.29578, tolerance = MAXERROR))
  expect_that(rad2deg( pi/2), equals( 90, tolerance = MAXERROR))
  expect_that(rad2deg( pi  ), equals(180, tolerance = MAXERROR))
})

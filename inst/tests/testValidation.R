##
##  Test for validation functions
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

context("Validate functions")

test_that("validate.Latitude validation", {
  expect_that(validate.Latitude(   0), equals(  0))
  expect_that(validate.Latitude(  30), equals( 30))
  expect_that(validate.Latitude( 390), equals( 30))
  expect_that(validate.Latitude( 750), equals( 30))
  expect_that(validate.Latitude( -30), equals(-30))
  expect_that(validate.Latitude( 150), equals( 30))
  expect_that(validate.Latitude( 330), equals(-30))
})

test_that("validate.Longitude validation", {
  expect_that(validate.Longitude(   0), equals(  0))
  expect_that(validate.Longitude(  30), equals( 30))
  expect_that(validate.Longitude( 210), equals( 30))
  expect_that(validate.Longitude( 390), equals( 30))
  expect_that(validate.Longitude( -30), equals(-30))
  expect_that(validate.Longitude( 330), equals(150))
  expect_that(validate.Longitude( 510), equals(150))
})

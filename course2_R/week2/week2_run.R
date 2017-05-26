rm(list=ls())

library(testthat)
source("c:/work/r/week2/week2_assignment.R")

context('Test id parameter')

test_that("validate_id_parameter validates ID Parameters!",
          {
                  expect_that(validate_id_parameter(1:1), equals(c(1)))
                  expect_that(validate_id_parameter(1:2), equals(c(1,2)))
                  expect_that(validate_id_parameter(1:5), equals(c(1,2,3,4,5)))
                  expect_that(validate_id_parameter(c(1,2,5)), equals(c(1,2,5)))
          })

context('Test PART 1')
test_that("results for part 1 equate to suggested values",
          {
                  expect_that(round(pollutantmean('specdata', "sulfate", 1:10), digits = 3), equals(4.064))
                  expect_that(round(pollutantmean("specdata", "nitrate", 70:72), digits = 3), equals(1.706))
                  expect_that(round(pollutantmean("specdata", "nitrate", 23), digits = 3), equals(1.281))
          })

context('Test PART 2')
test_that("results for part 1 equate to suggested values",
          {
                  expect_that(complete('specdata', 1), equals("##id nobs## 1  1  117"))

          })


# source(

# Part 2

# result <- complete("specdata", 1)
# print(result)
##   id nobs
## 1  1  117
##
# complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
##
#complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
##
# complete("specdata", 3)
##   id nobs
## 1  3  243




# Part 1

# result <- pollutantmean('specdata', "sulfate", 1:10)
# if ( round(result, digits = 3) == 4.064 ) { print('Test 1 pass')}
#
# result <- pollutantmean("specdata", "nitrate", 70:72)
# if ( round(result, digits = 3) == 1.706 ) { print('Test 2 pass')}
#
# result <- pollutantmean("specdata", "nitrate", 23)
# if ( round(result, digits = 3) == 1.281 ) { print('Test 3 pass')}

setwd("C:/Users/Lydia/Documents/Stat159/lab7/test-that")
library("testthat")

x <- c(1, 2, 3, 4, 5)
y <- c(1, 2, 3, 4, NA)
z <- c(TRUE, FALSE, TRUE)
w <- letters[1:5]

# load the source code of the functions to be tested
source("./functions/range-value.R")
source("./functions/missing-values.R")

context("Test for range value") 
test_that("range works as expected", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_equal(range_value(y), NA_real_)
  expect_length(range_value(y), 1)
})

context("Test for range value") 
test_that("range works as expected", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(range_value(z), 1L)
  expect_length(range_value(z), 1)
  expect_type(range_value(z), "integer")
})

context("Test for range value") 
test_that("range works as expected", {
  w <- letters[1:5]
  expect_error(range_value(w))
})

context("test for missing values")
test_that("missing values works as expected", {
  y <- c(1, 2, 3, 4, NA)
  expect_length(missing_values(y), 1)
})

context("test for missing values")
test_that("missing values works as expected", {
  y <- c(1, 2, 3, 4, NA)
  expect_type(missing_values(y), "integer")
})

context("test for missing values")
test_that("missing values works as expected", {
  y <- c(1, 2, 3, 4, NA)
  expect_more_than(missing_values(y), 0)
})


range_value <- function(x, na.rm = TRUE) {
  if (na.rm == FALSE) {
    max(x) - min(x)
    }
  else {
    max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  }
}

context("Test for range value") 
test_that("range works as expected", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_equal(range_value(y), 3)
  expect_length(range_value(y), 1)
})



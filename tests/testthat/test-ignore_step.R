library(testthat)
library(recipes)
library(dplyr)

rec <- recipe(Species ~ ., data = iris) %>%
  step_rm(Petal.Width, id = "rm_UDLut") %>%
  step_rm(starts_with("Sepal"), id = "custom_id")

test_that("ignore_step outputs a recipe", {
  expect_equal(class(ignore_step(rec, "rm")), "recipe")
  expect_equal(class(ignore_step(rec, 1)), "recipe")
  expect_equal(class(ignore_step(rec, rec$steps[[1]]$id)), "recipe")
})

test_that("ignore_step removes the right number of steps", {
  expect_length(ignore_step(rec, "rm")$steps, 0)
  expect_length(ignore_step(rec, 1)$steps, 1)
  expect_length(ignore_step(rec, 1:2)$steps, 0)
})

test_that("function returns error given certain inputs", {
  expect_error(ignore_step(rec, "normalize"))
  expect_error(ignore_step(rec, c("rm", "normalize")))
  expect_error(ignore_step(rec, 5))
  expect_error(ignore_step(rec, c(1, 5)))
})


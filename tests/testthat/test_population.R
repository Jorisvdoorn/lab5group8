context("population")

test_that("population rejects errounous input", {
  expect_error(population_mod <- population$new())
})

test_that("class is correct", {
  population_mod <- population$new()
  
  expect_true(class(population_mod)[1] == "population")
})


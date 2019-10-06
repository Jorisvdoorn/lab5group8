context("koladaApp")

test_that(" 'kolada' is an object", {
  expect_is(kolada, "population")
})

test_that(" 'list_municip' is character", {
  expect_equal(typeof(list_municip), "character")
})

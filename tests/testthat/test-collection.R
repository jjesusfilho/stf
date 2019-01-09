context("get_stf_collection")

test_that("returns a data.frame", {
  expect_is(get_stf_collection(decision_type = "colegiadas", years = 2014), "data.frame")
})

context("get_stf_collection")

test_that("effectvelly retrieves data collections", {
  expect_equal(length(get_stf_collection(decision_type = "colegiadas", years = 2016)), 19)
})

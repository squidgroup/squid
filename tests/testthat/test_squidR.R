context("squidR input arguments")

test_that("squidR returns error when input is not a list()", {
  expect_error(squidR(input=c(0,1,2,3)), "input must be a list.")
  expect_error(squidR(input="test"), "input must be a list.")
  # expect_error(squidR(input=data.frame("test"=c(1,2,3,4))), "input must be a list.")
  expect_error(squidR(input=matrix(c(1,2,3,4), ncol=2)), "input must be a list.")
  expect_error(squidR(input=factor(c(1,2,3,4))), "input must be a list.")
})

test_that("squidR returns error when plot is not logical", {
  expect_error(squidR(plot=c(0,1,2,3)), "plot must be logical.")
  expect_error(squidR(plot="test"), "plot must be logical.")
  expect_error(squidR(plot=data.frame("test"=c(1,2,3,4))), "plot must be logical.")
  expect_error(squidR(plot=matrix(c(1,2,3,4), ncol=2)), "plot must be logical.")
  expect_error(squidR(plot=factor(c(1,2,3,4))), "plot must be logical.")
})

test_that("squidR returns error when data is not data.frame from squidR output", {
  expect_error(squidR(data=c(0,1,2,3)), "data must be the full data returned by squidR (output$full_data).")
  expect_error(squidR(data="test"), "data must be the full data returned by squidR (output$full_data).")
  expect_error(squidR(data=data.frame("test"=c(1,2,3,4))), "data must be the full data returned by squidR (output$full_data).")
  expect_error(squidR(data=matrix(c(1,2,3,4), ncol=2)), "data must be the full data returned by squidR (output$full_data).")
  expect_error(squidR(data=factor(c(1,2,3,4))), "data must be the full data returned by squidR (output$full_data).")
})






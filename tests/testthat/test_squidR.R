context("squidR input arguments")

test_that("squidR returns a list of 2 data.frames when no arguments are passed", {
  expect_is(squidR(), "list")
  expect_equal(length(squidR()), 2)
  expect_equal(dim(squidR()$full_data), c(1,19))
  expect_equal(dim(squidR()$sampled_data), c(1,19))
})

test_that("squidR returns a list of 2 data.frames when only module name is specified", {
  expect_is(squidR(module="test"), "list")
  expect_equal(length(squidR(module="test")), 2)
  expect_equal(dim(squidR(module="test")$full_data), c(1,19))
  expect_equal(dim(squidR(module="test")$sampled_data), c(1,19))
})

test_that("squidR returns a list of 2 data.frames and 1 list of ggplot figures when only plot is TRUE", {
  expect_is(squidR(plot=TRUE), "list")
  expect_equal(length(squidR(plot=TRUE)), 3)
  expect_equal(dim(squidR(plot=TRUE)$full_data), c(1,19))
  expect_equal(dim(squidR(plot=TRUE)$sampled_data), c(1,19))
  expect_equal(length(squidR(plot=TRUE)$plot), 6)
})

test_that("squidR returns a ggplot figure when X_previsualization is specified", {
  expect_is(squidR(X_previsualization="X1"), "ggplot")
  expect_is(squidR(X_previsualization="X2"), "ggplot")
  expect_error(squidR(X_previsualization="X12"), "X_previsualization is not a valid environment tag.")
})

test_that("squidR returns error when input is not a list()", {
  expect_error(squidR(input=c(0,1,2,3)), "input must be a list.")
  expect_error(squidR(input="test"), "input must be a list.")
  # expect_error(squidR(input=data.frame("test"=c(1,2,3,4))), "input must be a list.")
  expect_error(squidR(input=matrix(c(1,2,3,4), ncol=2)), "input must be a list.")
  expect_error(squidR(input=factor(c(1,2,3,4))), "input must be a list.")
})



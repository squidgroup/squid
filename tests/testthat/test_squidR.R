context("squidR input arguments")

test_that('Error when "input" is not a list', {
  expect_error(squidR(input=c(0,1,2,3)), "input must be a list.")
  expect_error(squidR(input="test"), "input must be a list.")
  # expect_error(squidR(input=data.frame("test"=c(1,2,3,4))), "input must be a list.")
  expect_error(squidR(input=matrix(c(1,2,3,4), ncol=2)), "input must be a list.")
  expect_error(squidR(input=factor(c(1,2,3,4))), "input must be a list.")
})

plot_error_message <- "plot must be one logical element."
test_that('Error when "plot" is not logical', {
  expect_error(squidR(plot=c(0,1,2,3)), plot_error_message)
  expect_error(squidR(plot="test"), plot_error_message)
  expect_error(squidR(plot=data.frame("test"=c(1,2,3,4))), plot_error_message)
  expect_error(squidR(plot=matrix(c(1,2,3,4), ncol=2)), plot_error_message)
  expect_error(squidR(plot=factor(c(1,2,3,4))), plot_error_message)
  expect_error(squidR(plot=c(TRUE, FALSE)), plot_error_message)
})

test_that('Error when "data" is not data.frame from squidR output', {
  expect_error(squidR(data=c(0,1,2,3)), "data must be the full data returned by squidR.")
  expect_error(squidR(data="test"), "data must be the full data returned by squidR.")
  expect_error(squidR(data=data.frame("test"=c(1,2,3,4))), "data must be the full data returned by squidR.")
  expect_error(squidR(data=matrix(c(1,2,3,4), ncol=2)), "data must be the full data returned by squidR.")
  expect_error(squidR(data=factor(c(1,2,3,4))), "data must be the full data returned by squidR.")
})

module_error_message <- "module must be a one string element."
test_that('Error when "module" is not a string', {
  expect_error(squidR(module=c(0,1,2,3)), module_error_message)
  expect_error(squidR(module=1), module_error_message)
  expect_error(squidR(module=TRUE), module_error_message)
  expect_error(squidR(module=data.frame("test"=c(1,2,3,4))), module_error_message)
  expect_error(squidR(module=matrix(c(1,2,3,4), ncol=2)), module_error_message)
  expect_error(squidR(module=factor(c(1,2,3,4))), module_error_message)
  expect_error(squidR(module=c("module1", "module2")), module_error_message)
})

X_previsualization_error_message <- "X_previsualization is not a valid environment tag."
test_that('Error when "X_previsualization" is not a valid environment tag', {
  expect_error(squidR(X_previsualization=c(0,1,2,3)), X_previsualization_error_message)
  expect_error(squidR(X_previsualization=1), X_previsualization_error_message)
  expect_error(squidR(X_previsualization=TRUE), X_previsualization_error_message)
  expect_error(squidR(X_previsualization=data.frame("test"=c(1,2,3,4))), X_previsualization_error_message)
  expect_error(squidR(X_previsualization=matrix(c(1,2,3,4), ncol=2)), X_previsualization_error_message)
  expect_error(squidR(X_previsualization=factor(c(1,2,3,4))), X_previsualization_error_message)
  expect_error(squidR(X_previsualization=c("X1", "X2")), X_previsualization_error_message)
})



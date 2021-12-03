test_that("my.summary prints", {
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)))), "Call:")
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)))), "Coefficients:")
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)))), "Residual standard error:")
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)))), "F-statistic:")
})

test_that("my.summary prints OLS", {
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)))), "Call:")
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)))), "Coefficients:")
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)))), "Residual standard error:")
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)))), "F-statistic:")
})

test_that("my.summary prints WLS", {
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)), "Call:")
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)), "Coefficients:")
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)), "Residual standard error:")
  expect_output(my.summary(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)), "F-statistic:")
})

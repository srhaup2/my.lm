test_that("OLS works with 1 predictor", {
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)))$coefficients), as.numeric(lm(mpg ~ cyl, data = get(data(mtcars)))$coefficients))
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)))$residuals), as.numeric(lm(mpg ~ cyl, data = get(data(mtcars)))$residuals))
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)))$fitted.values), as.numeric(lm(mpg ~ cyl, data = get(data(mtcars)))$fitted.values))
  expect_equal(my.lm(mpg ~ cyl, data = get(data(mtcars)))$r2, summary(lm(mpg ~ cyl, data = get(data(mtcars))))$r.squared)
  expect_equal(my.lm(mpg ~ cyl, data = get(data(mtcars)))$adj.r2, summary(lm(mpg ~ cyl, data = get(data(mtcars))))$adj.r.squared)
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)))$f), as.numeric(summary(lm(mpg ~ cyl, data = get(data(mtcars))))$fstatistic[1]))
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)))$t.p.vals), as.numeric(summary(lm(mpg ~ cyl, data = get(data(mtcars))))$coefficients[,4]))
})


test_that("OLS works with multiple predictors, incl transformations", {
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$coefficients), as.numeric(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$coefficients))
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$residuals), as.numeric(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$residuals))
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$fitted.values), as.numeric(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$fitted.values))
  expect_equal(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$r2, summary(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars))))$r.squared)
  expect_equal(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$adj.r2, summary(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars))))$adj.r.squared)
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$f), as.numeric(summary(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars))))$fstatistic[1]))
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)))$t.p.vals), as.numeric(summary(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars))))$coefficients[,4]))
})

test_that("WLS works with 1 predictor", {
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$coefficients), as.numeric(lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$coefficients))
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$residuals), as.numeric(lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$residuals))
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$fitted.values), as.numeric(lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$fitted.values))
  expect_equal(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$r2, summary(lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32))$r.squared)
  expect_equal(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$adj.r2, summary(lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32))$adj.r.squared)
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$f), as.numeric(summary(lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32))$fstatistic[1]))
  expect_equal(as.numeric(my.lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32)$t.p.vals), as.numeric(summary(lm(mpg ~ cyl, data = get(data(mtcars)), weights = 1:32))$coefficients[,4]))
})

test_that("WLS works with multiple predictors, incl transformations", {
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$coefficients), as.numeric(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$coefficients))
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$residuals), as.numeric(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$residuals))
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$fitted.values), as.numeric(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$fitted.values))
  expect_equal(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$r2, summary(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32))$r.squared)
  expect_equal(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$adj.r2, summary(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32))$adj.r.squared)
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$f), as.numeric(summary(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32))$fstatistic[1]))
  expect_equal(as.numeric(my.lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32)$t.p.vals), as.numeric(summary(lm(mpg ~ cyl + I(cyl^2) + log(hp), data = get(data(mtcars)), weights = 1:32))$coefficients[,4]))
})

#'my.summary
#'
#'prints regression summary from output of my.lm function
#'
#'@param x output from my.lm function
#'
#'@return no return value - this function only cats a summary of the regression
#'
#'@examples
#'get(data(mtcars))
#'my.fit = my.lm(mpg ~ cyl, data = mtcars)
#'summary(my.fit)
#'
#'@export
#'

my.summary = function(x){
  ###cat summary
  cat("Call:\n")
  print(x$formula)
  cat("\n")
  ###get data on residuals
  # check if WLS or OLS
  if (x$method == "OLS") {
    mi = min(x$residuals)
    q1 = stats::quantile(x$residuals, 0.25)
    med = stats::median(x$residuals)
    q3 = stats::quantile(x$residuals, 0.75)
    ma = max(x$residuals)
    res_vec = round(c(mi, q1, med, q3, ma),4)
    names(res_vec) = c("Min", "1Q", "Median", "3Q", "Max")
    cat("Residuals: \n")
    print(res_vec)
    cat("\n")
  } else {
    w_res = x$res *sqrt(x$weights)
    mi = min(w_res)
    q1 = stats::quantile(w_res, 0.25)
    med = stats::median(w_res)
    q3 = stats::quantile(w_res, 0.75)
    ma = max(w_res)
    w_res_vec = round(c(mi, q1, med, q3, ma),4)
    names(w_res_vec) = c("Min", "1Q", "Median", "3Q", "Max")
    cat("Weighted Residuals: \n")
    print(w_res_vec)
    cat("\n")
  }

  ###create summary table
  tbl = data.frame(Estimate = as.numeric(x$coefficients), Std.Error = as.numeric(x$se_beta),
                   t.value = as.numeric(x$t), p.value = as.numeric(x$t.p.vals))
  rownames(tbl) = rownames(x$coefficients)
  sigcodes = ifelse(x$t.p.value < 0.001, "***",
                    ifelse(x$t.p.value < 0.01, "**",
                           ifelse(x$t.p.value < 0.05, "*",
                                  ifelse(x$t.p.value < 0.1, ".", ""))))
  tbl = signif(rbind(tbl,sigcodes),4)
  cat("Coefficients:\n")
  print(tbl)
  cat("---\n")
  cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")
  cat("\n")
  ###other statistics
  cat(paste0("Residual standard error: ", round(sqrt(x$mse),4)," on ", round(x$df.residual,4), " degrees of freedom\n"))
  cat(paste0("Multiple R-squared: ", round(x$r2,4)," ,      Adjusted R-squared: ", round(x$adj.r2,4), "\n"))
  cat(paste0("F-statistic: ", round(x$f,4)," on ", length(x$coefficients) - 1, " and ", x$df.residual, " DF,  p-value: ", signif(x$f.p.val,4)))
}

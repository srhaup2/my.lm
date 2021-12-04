#'my.summary
#'
#'prints regression summary from output of my.lm function
#'
#'@param x output from my.lm function
#'
#'@return no return value - this function only prints a summary of the regression
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
  cat("Call\u003a\u000a")
  print(x$formula)
  cat("\u000a")
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
    cat("Residuals\u003a \u000a")
    print(res_vec)
    cat("\u000a")
  } else {
    w_res = x$res * (x$weights)
    mi = min(w_res)
    q1 = stats::quantile(w_res, 0.25)
    med = stats::median(w_res)
    q3 = stats::quantile(w_res, 0.75)
    ma = max(w_res)
    w_res_vec = round(c(mi, q1, med, q3, ma),4)
    names(w_res_vec) = c("Min", "1Q", "Median", "3Q", "Max")
    cat("Weighted Residuals\u003a \u000a")
    print(w_res_vec)
    cat("\u000a")
  }

  ###create summary table
  tbl = data.frame(Estimate = as.numeric(x$coefficients), Std.Error = as.numeric(x$se_beta),
                   t.value = as.numeric(x$t), p.value = as.numeric(x$t.p.vals))
  rownames(tbl) = rownames(x$coefficients)
  sigcodes = ifelse(x$t.p.value < 0.001, "\u002a\u002a\u002a",
                    ifelse(x$t.p.value < 0.01, "\u002a\u002a",
                           ifelse(x$t.p.value < 0.05, "\u002a",
                                  ifelse(x$t.p.value < 0.1, ".", ""))))
  tbl = signif(rbind(tbl,sigcodes),4)
  cat("Coefficients\u003a\u000a")
  print(tbl)
  cat("\u002d\u002d\u002d\u000a")
  cat("Signif. codes\u003a  0 \u00e2\u002a\u002a\u002a\u00e2 0.001 \u00e2\u002a\u002a\u00e2 0.01 \u00e2\u002a\u00e2 0.05 \u00e2.\u00e2 0.1 \u00e2 \u00e2 1\u000a")
  cat("\u000a")
  ###other statistics
  cat(paste0("Residual standard error\u003a ", round(sqrt(x$mse),4)," on ", round(x$df.residual,4), " degrees of freedom\u000a"))
  cat(paste0("Multiple R\u002dsquared\u003a ", round(x$r2,4)," ,      Adjusted R\u002dsquared\u003a ", round(x$adj.r2,4), "\u000a"))
  cat(paste0("F\u002dstatistic\u003a ", round(x$f,4)," on ", length(x$coefficients) - 1, " and ", x$df.residual, " DF,  p\u002dvalue\u003a ", signif(x$f.p.val,4)))
}

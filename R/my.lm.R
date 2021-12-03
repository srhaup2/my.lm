#'my.lm
#'
#'Fits a linear regression model using OLS
#'
#'@param formula formula specifying the regression equation
#'
#'@param data data.frame to be used to fit the model
#'
#'@param weights optional numeric vector to be used in a WLS regression
#'
#'@return A list containing the coefficients, residuals, fitted values, and more
#'
#'@examples
#'get(data(mtcars))
#'my.fit = my.lm(mpg ~ cyl, data = mtcars)
#'my.fit$coefficients
#'
#'@export
#'

my.lm = function(formula, data, weights) {
  ### load pipe
  `%>%` <- magrittr::`%>%`
  ### extract formula
  vars = all.vars(formula)
  nvar = length(vars)
  y.name = vars[1]
  x.names = vars[2:nvar]
  ### create model.frame
  mf= stats::model.frame(formula = formula, data = data)
  y = mf[,1]
  ### create design matrix
  mm = stats::model.matrix(formula, mf)
  n = nrow(mm)
  p = ncol(mm)
  ### WLS or OLS
  if(!missing("weights")) {
    #use WLS estimator
    w = diag(weights)
    coefficients = solve(t(mm) %*% w %*% mm) %*% (t(mm) %*% w %*% y)
    fitted.values = mm %*% coefficients
    residuals = y - fitted.values
    sse = as.numeric(t(y) %*% w %*% y - t(y) %*% w %*% mm %*% solve(t(mm) %*% w %*% mm) %*% (t(mm) %*% w %*% y))
    # R uses this different method for computing ssy in WLS (very confusing)
    ssy = sum(weights*(y^2)) - (1/sum(weights))*(sum(weights*y)^2)
    ssr = ssy-sse
    mse = sse/(n-p)
    msr = ssr/(p-1)
    msy = ssy/(n-1)
    var_y = mse * solve(w)
    var_beta = solve(t(mm) %*% w %*% mm) %*% t(mm) %*% w %*% var_y %*% t(w) %*% mm %*% solve(t(mm) %*% w %*% mm)
    sigma = sqrt(mse)
    se_beta = sqrt(diag(var_beta))
  } else {
    #use OLS estimator
    coefficients = solve(t(mm) %*% mm) %*% (t(mm) %*% y)
    fitted.values = mm %*% coefficients
    residuals = y - fitted.values
    sse = as.numeric(crossprod(residuals))
    ssr = as.numeric(crossprod(fitted.values - mean(y)))
    ssy = as.numeric(crossprod(y - mean(y)))
    mse = sse/(n-p)
    msr = ssr/(p-1)
    msy = ssy/(n-1)
    var_beta = mse * solve(t(mm) %*% mm)
    sigma = sqrt(mse)
    se_beta = sqrt(diag(var_beta))
  }
  ### calculate other relevant things
  r2 = ssr/ssy
  adj.r2 = 1 - ((sse/(n-p))/(ssy/(n-1)))
  #t stats
  t = coefficients/se_beta
  t.p.vals = ifelse(t > 0, (2* stats::pt(t, df = 29, lower.tail = FALSE)), (2* stats::pt(t, df = 29, lower.tail = TRUE)))
  #f stat
  f = msr/mse
  f.p.val = stats::pf(f, p-1, n-p, lower.tail = FALSE)
  return(list(coefficients = coefficients, residuals = residuals, fitted.values = fitted.values,
              mse = mse, se_beta = se_beta, r2 = r2, adj.r2 = adj.r2, t = t, t.p.vals = t.p.vals,
              f = f, f.p.val = f.p.val,df.residual = n - p, formula = formula, design.mat = mm))
}

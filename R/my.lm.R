#'my.lm
#'
#'Fits a linear regression model using OLS
#'
#'@param x input value
#'
#'@return the first j principal components
#'
#'@examples
#'
#'
#'@export
#'

my.lm = function(formula, data, subset, weights, na.action) {



  return(list(coefficients = coefficients, residuals = residuals, fitted.values = fitted.values,
              df.residual = df.residual, call = call, terms = terms, model = model))
}

#'my.lm
#'
#'Fits a linear regression model using OLS
#'
#'@param formula formula specifying the regression equation
#'
#'@param data data.frame to be used to fit the model
#'
#'@param subset optional logical vector specifying rows to be used to fit the model
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

my.lm = function(formula, data, subset, weights, na.action) {
  ### load pipe
  `%>%` <- magrittr::`%>%`
  ### check if we need to subset data
  if (!is.null(subset)) {
    data = data %>%
      dplyr::filter(subset)
  }
  ### extract formula
  vars = all.vars(formula)
  nvar = length(vars)
  y = vars[1]
  x = vars[2:nvars]
  ### create model.frame
  m = model.frame(formula, data)
  ### create dummy variables


  ### WLS or OLS


  return(list(coefficients = coefficients, residuals = residuals, fitted.values = fitted.values,
              df.residual = df.residual, call = call, terms = terms, model = model))
}

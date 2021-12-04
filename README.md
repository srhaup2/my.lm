<!-- badges: start -->
[![R-CMD-check](https://github.com/srhaup2/my.lm/workflows/R-CMD-check/badge.svg)](https://github.com/srhaup2/my.lm/actions)
[![codecov](https://codecov.io/gh/srhaup2/my.lm/branch/main/graph/badge.svg?token=fRJzChk3Vm)](https://codecov.io/gh/srhaup2/my.lm)
<!-- badges: end -->

### Description
------------

`my.lm` is a package I created for the class Biostats 625 - Computing with Big Data I took during my MS Biostatistics degree. 
The objective of the project was to implement a statistical technique and wrap it up in an R package.
I decided to re-implement the main functionality of R's `lm` function and its associated `summary()` method.

`lm` implements linear regression models using either ordinary least squares (OLS) or weighted least squares (WLS).
My function, `my.lm` implements the most important functionality of `lm`, and `my.summary` mimics `lm`'s `summary()` method. 

### Installation
------------

To install `my.lm`, download directly from this Github repository using:

``` r
install.packages('devtools')
devtools::install_github('srhaup2/my.lm', build_vignettes = T)
```

and load the functions in this package using:

``` r 
library(my.lm)
```

### Example
-------

Here is a quick example of how to use `my.lm`

``` r
library(my.lm)

#load in data
get(data(mtcars))

#fit a linear regression model with OLS
fit_OLS = my.lm(mpg ~ cyl, data = mtcars)

#print summary
my.summary(fit_OLS)

#fit a linear regression model with WLS
fit_WLS = my.lm(mpg ~ cyl, data = mtcars, weights = 1:32)

#print summary
my.summary(fit_WLS)
```

Further details of how these functions work is provided in the help pages, accessed with:

``` r
?my.lm
?my.summary
```

Even more details about linear regression and `my.lm` functionality and performance is included in the vignette:

``` r
browseVignettes(package = 'my.lm')
```

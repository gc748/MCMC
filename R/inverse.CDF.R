# @param invCDF : inverse CDF function that accepts 'u'
#   Example: my.invCDF <- function(u) qnorm(u, 0, 1)
# @param n : desired sampled size

inverse.CDF <- function(invCDF, n)
{
  u <- runif(n)
  return(invCDF(u))
}

# @example
# my.invCDF <- function(u) qnorm(u, 0, 1)
# x <- inverse.CDF(invCDF = my.invCDF, n=1000)
# hist(x)

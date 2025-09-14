# @param target.pdf : the target probability density function as a function of
#                     'x'
#   Example : target.pdf <- function(x) dbeta(x, 12, 6)
# @param lower : target pdf lower bound
# @param upper : target pdf upper bound
# @param n : desired sample size

accept.reject <- function(target.pdf, lower, upper, n)
{
  ### Estimate M
  M <- max(target.pdf(seq(lower, upper, length.out = 1e5)))
  ### Storage
  x <- numeric()
  ### Loop
  while(length(x) < n)
  {
    x.star <- runif(1, lower, upper)
    y <- runif(1, 0, M)
    if(y <= target.pdf(x.star)) x <- c(x, x.star)
  }
  return(x)
}

# @example
# target.pdf <- function(x) dbeta(x, 12, 6)
# x <- accept.reject(target.pdf = target.pdf, lower = 0, upper = 1, n = 1000)
# hist(x)

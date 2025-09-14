# @param target.df : the target probability density function as a function of
#                    'x'
#   Example : target.df <- function(x) dexp(x, 1)
# @param prop.fun : proposal function that generates a perturbation from a
#                   given 'x'
#   Example : prop.fun <- function(x) x + rnorm(1, 0, 0.1)
# @param start : a numerical input; starting point of MCMC chain
# @param nsteps : a numerical input; number of MCMC steps
#
# @note : This MCMC has a slightly different method for generating results.
#         It is written for multiple dimensions. You may use it in this form,
#         but should only give one-dimensionsal proposal functions.

MCMC.metropolis <- function(target.df, prop.fun, start, nsteps)
{
  ### Perturbation step function
  step <- function(x, target.df, prop.fun)
  {
    x.prop <- prop.fun(x)
    alpha <- target.df(x.prop) / target.df(x)
    if(runif(1) < alpha) x <- x.prop
    return(x)
  }
  ### Storage
  x <- start
  results <- matrix(NA, nsteps, length(start))
  ### Loop
  for(i in seq_len(nsteps))
  {
    results[i,] <- x <- step(x, target.df, prop.fun)
  }
  return(results)
}

# @example
# target.df <- function(x) dexp(x, 1)
# prop.fun <- function(x) x + rnorm(1, 0, 0.5)
# x <- MCMC.metropolis(target.df = target.df, prop.fun = prop.fun,
# start=runif(1, 0, 3), nsteps=1e3)
# hist(x)

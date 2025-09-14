# @param x : matrix of size n x m; m columns represent different MCMC chains of
# step-size n

Gelman <- function(x)
{
  n <- dim(x)[1]
  m <- dim(x)[2]
  ### Within-Chain Variance
  W <- mean(apply(x, 2, var))
  ### Between-Chain Variance
  B <- (n/(m-1))*sum((apply(x,2,mean)-mean(x))^2)
  ### Weighted Variance Estimate
  sigma2.hat <- ((n-1)/n)*W + (1/n)*B
  ### Gelman Ratio
  R.hat <- sqrt(sigma2.hat/W)
  ### Number of Effective Samples
  n.eff <- m*n*sigma2.hat/B
  return(data.frame(W=W, B=B, sigma2.hat=sigma2.hat, R.hat=R.hat, n.eff=n.eff))
}

# @example
# target.df <- function(x) dexp(x, 1)
# prop.fun <- function(x) x + rnorm(1, 0, 0.5)
# results <- matrix(nrow = 1e3, ncol = 9)
# for(j in 1:9)
# results[,j] <- MCMC.metropolis(target.df, prop.fun, start=runif(1, 0, 3),
# nsteps=1e3)
# Gelman(results)

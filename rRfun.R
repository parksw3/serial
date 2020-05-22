serialR <- function(r=0.1,
                    rho=0.5,
                    meanlog1=1.62,
                    sdlog1=0.42,
                    meanlog2=1.54,
                    sdlog2=0.37,
                    nsim=100000,
                    seed=101) {
  set.seed(seed)
  backinc <- rlnorm(nsim, meanlog=meanlog1, sdlog=sdlog1)
  
  backinc <- sample(backinc, replace=TRUE, prob=exp(-r*backinc))
  
  logbackinc <- log(backinc)
  
  muloggen <- meanlog2 + sdlog2 * rho * (logbackinc - meanlog1)/sdlog1
  sigmaloggen <- sqrt(sdlog2^2 * (1 - rho^2))
  
  forwardgen <- exp(rnorm(nsim, muloggen, sigmaloggen))
  forwardinc <- rlnorm(nsim, meanlog=meanlog1, sdlog=sdlog1)
  
  ser <- -backinc + forwardgen + forwardinc
  
  list(
    mean=mean(ser),
    kappa=var(ser)/mean(ser)^2,
    R=1/mean(exp(-r*ser))
  )
}

serialR2 <- function(r=0.1,
                     rho=0.5,
                     meanlog1=1.62,
                     sdlog1=0.42,
                     meanlog2=1.54,
                     sdlog2=0.37,
                     nsim=100000,
                     seed=101) {
  set.seed(seed)
  backinc <- rlnorm(nsim, meanlog=meanlog1, sdlog=sdlog1)
  
  logbackinc <- log(backinc)
  
  muloggen <- meanlog2 + sdlog2 * rho * (logbackinc - meanlog1)/sdlog1
  sigmaloggen <- sqrt(sdlog2^2 * (1 - rho^2))
  
  forwardgen <- exp(rnorm(nsim, muloggen, sigmaloggen))
  forwardinc <- rlnorm(nsim, meanlog=meanlog1, sdlog=sdlog1)
  
  ser <- -backinc + forwardgen + forwardinc
  
  list(
    mean=mean(ser),
    kappa=var(ser)/mean(ser)^2,
    R=1/mean(exp(-r*ser))
  )
}

generationR <- function(r=0.1,
                        meanlog2=1.54,
                        sdlog2=0.37,
                        nsim=100000,
                        seed=101) {
  set.seed(101)
  gen <- rlnorm(nsim, meanlog2, sdlog2)
  
  list(
    mean=mean(gen),
    kappa=var(gen)/mean(gen)^2,
    R=1/mean(exp(-r*gen))
  )
}
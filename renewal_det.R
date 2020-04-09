renewal_det <- function(R0=2.5,
                        N=40000,
                        S0=40000-10,
                        dt=0.1,
                        incfun=function(x) dlnorm(x, meanlog=1.621, sdlog=0.418),
                        genfun=function(x) dweibull(x, shape=4.1, scale=5.5),
                        I0=10,
                        tmax=150,
                        genmax=150) {
  inc <- incfun(0:genmax*dt)
  inc <- inc/sum(inc)
  gen <- genfun(0:genmax*dt)
  gen <- gen/sum(gen)
  tvec <- seq(0, tmax, by=dt)
  Ivec <- rep(0, dt*tmax)
  
  r <- optim(0.19, function(x) (1/sum(exp(-x*(0:genmax*dt)) * gen)-R0)^2,
        lower=0.1,
        upper=0.3,
        method="Brent")[[1]]
  
  ## r=0.1904075 for R0=2.5
  Ivec[1:genmax] <- exp(r*1:genmax*dt)
  Ivec[1:genmax] <- I0*Ivec[1:genmax]/sum(Ivec[1:genmax])
  Svec <- rep(0, dt*tmax)
  Svec[1:genmax] <- N - cumsum(Ivec)
  
  for (i in (genmax+1):length(tvec)) {
    Ivec[i] <- Svec[i-1] * R0 * sum(Ivec[max(1, i-genmax):(i-1)] * gen[min(i, genmax+1):2])/N
    Svec[i] <- Svec[i-1] - Ivec[i]
  }
  
  forwardgen <- sapply(1:(length(Svec)-genmax), function(x) gen[1:(1+genmax)]*Svec[x:(x+genmax)]/sum(gen[1:(1+genmax)]*Svec[x:(x+genmax)]))
  backwardgen <- sapply((genmax+1):(length(Svec)), function(x) gen[1:(1+genmax)]*Ivec[x:(x-genmax)]/sum(gen[1:(1+genmax)]*Ivec[x:(x-genmax)]))
  forwardinc <- sapply(1:(length(Svec)-genmax), function(x) inc)
  backwardinc <- sapply((genmax+1):(length(Svec)), function(x) inc[1:(1+genmax)]*Ivec[x:(x-genmax)]/sum(inc[1:(1+genmax)]*Ivec[x:(x-genmax)]))
  
  list(
    tvec=head(tvec, -2*genmax),
    cI=tail(head(cumsum(Ivec), -genmax), -genmax),
    Ivec=tail(head(Ivec, -genmax), -genmax),
    Svec=tail(head(Svec, -genmax), -genmax),
    mfgen=tail(apply(forwardgen, 2, function(x) sum(x*0:genmax*dt)/sum(x)), -genmax),
    mbgen=head(apply(backwardgen, 2, function(x) sum(x*0:genmax*dt)/sum(x)), -genmax),
    mfinc=tail(apply(forwardinc, 2, function(x) sum(x*0:genmax*dt)/sum(x)), -genmax),
    mbinc=head(apply(backwardinc, 2, function(x) sum(x*0:genmax*dt)/sum(x)), -genmax),
    gen=gen,
    R0=R0,
    r=r
  )
}

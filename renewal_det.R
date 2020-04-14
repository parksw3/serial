renewal_det <- function(R0=2.5,
                        N=40000,
                        S0=40000-10,
                        dt=0.025,
                        incfun=function(x) dlnorm(x, meanlog=1.621, sdlog=0.418),
                        genfun=function(x) dlnorm(x, meanlog=1.54, sdlog=0.37),
                        I0=10,
                        tmax=200,
                        genmax=2000) {
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
  
  mfgen <- apply(forwardgen, 2, function(x) sum(x*0:genmax*dt)/sum(x))
  mbgen <- apply(backwardgen, 2, function(x) sum(x*0:genmax*dt)/sum(x))
  mfinc <- apply(forwardinc, 2, function(x) sum(x*0:genmax*dt)/sum(x))
  mbinc <- apply(backwardinc, 2, function(x) sum(x*0:genmax*dt)/sum(x))
  
  ## Jensen's inequality!!!!!
  # sum(backwardinc[,x-genmax]*rev(mfgen[(x-genmax):x]))-mbinc[1]
  
  #tmpmat <- matrix(0:(genmax)*dt, nrow=genmax+1, ncol=genmax+1) - t(matrix(0:genmax*dt, nrow=genmax+1, ncol=genmax+1))
  
  # mfser0 <- sapply((genmax+1):(length(Svec)-genmax), function(x) {
  #  sum(t(backwardinc[,x-genmax] * t(forwardgen[,x:(x-genmax)])) * tmpmat)
  #}) +
  #  tail(mfinc, -genmax)

  # sapply((genmax+1):(length(Svec)-genmax), function(x) sum(backwardinc[,x-genmax]*rev(mfgen[(x-genmax):x])))
    
  mfser <- head(sapply((genmax+1):(length(Svec)), function(x) sum(backwardinc[,x-genmax]*rev(mfgen[(x-genmax):x]))),-genmax) +
    tail(mfinc, -genmax) - head(mbinc, -genmax)
  
  # plot(mfser, type="l")
  
  mbgentmp <- c(rep(mbgen[1], genmax), mbgen)
  
  mbser <- head(sapply((genmax+1):(length(Svec)), function(x) sum(backwardinc[,x-genmax]*rev(mbgentmp[(x-genmax):x])))+mbinc,-genmax) -
    tail(mfinc, -genmax)
  
  list(
    tvec=head(tvec, -2*genmax),
    cI=tail(head(cumsum(Ivec), -genmax), -genmax),
    Ivec=tail(head(Ivec, -genmax), -genmax),
    Svec=tail(head(Svec, -genmax), -genmax),
    mfgen=tail(mfgen, -genmax),
    mbgen=head(mbgen, -genmax),
    mfinc=tail(mfinc, -genmax),
    mbinc=head(mbinc, -genmax),
    mfser=mfser,
    mbser=mbser,
    gen=gen,
    R0=R0,
    r=r
  )
}

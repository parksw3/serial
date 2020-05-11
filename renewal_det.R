renewal_det <- function(R0=2.5,
                        N=40000,
                        S0=40000-10,
                        dt=0.025,
                        incfun=function(x) dlnorm(x, meanlog=1.62, sdlog=0.42),
                        genfun=function(x) dlnorm(x, meanlog=1.54, sdlog=0.37),
                        I0=10,
                        tmax=200,
                        genmax=2000) {
  inc <- incfun(0:genmax*dt+dt)
  inc <- inc/sum(inc)
  gen <- genfun(0:genmax*dt+dt)
  gen <- gen/sum(gen)
  tvec <- seq(0, tmax, by=dt)
  Ivec <- rep(0, tmax/dt)
  
  r <- optim(0.19, function(x) (1/sum(exp(-x*(0:genmax*dt+dt)) * gen)-R0)^2,
        lower=0.1,
        upper=0.3,
        method="Brent")[[1]]
  
  ## r=0.1904075 for R0=2.5
  Ivec[1:genmax] <- exp(r*1:genmax*dt)
  Ivec[1:genmax] <- I0*Ivec[1:genmax]/sum(Ivec[1:genmax])
  Svec <- rep(0, tmax/dt)
  Svec[1:genmax] <- N - cumsum(Ivec)[1:genmax]
  
  for (i in (genmax+1):length(tvec)) {
    Ivec[i] <- Svec[i-1] * R0 * sum(Ivec[max(1, i-genmax):(i-1)] * gen[min(i, genmax+1):2])/N
    Svec[i] <- Svec[i-1] - Ivec[i]
  }
  
  forwardgen <- sapply(1:(length(Svec)-genmax), function(x) gen[1:(1+genmax)]*Svec[x:(x+genmax)]/sum(gen[1:(1+genmax)]*Svec[x:(x+genmax)]))
  backwardgen <- sapply((genmax+1):(length(Svec)), function(x) gen[1:(1+genmax)]*Ivec[x:(x-genmax)]/sum(gen[1:(1+genmax)]*Ivec[x:(x-genmax)]))
  forwardinc <- sapply(1:(length(Svec)-genmax), function(x) inc)
  backwardinc <- sapply((genmax+1):(length(Svec)), function(x) inc[1:(1+genmax)]*Ivec[x:(x-genmax)]/sum(inc[1:(1+genmax)]*Ivec[x:(x-genmax)]))
  
  mfgen <- apply(forwardgen, 2, function(x) sum(x*(0:genmax*dt+dt))/sum(x))
  mbgen <- apply(backwardgen, 2, function(x) sum(x*(0:genmax*dt+dt))/sum(x))
  mfinc <- apply(forwardinc, 2, function(x) sum(x*(0:genmax*dt+dt))/sum(x))
  mbinc <- apply(backwardinc, 2, function(x) sum(x*(0:genmax*dt+dt))/sum(x))
  
  mfser <- head(sapply((genmax+1):(length(Svec)), function(x) sum(backwardinc[,x-genmax]*rev(mfgen[(x-genmax):x]))),-genmax) +
    tail(mfinc, -genmax) - head(mbinc, -genmax)
  
  mbgentmp <- c(rep(mbgen[1], genmax), mbgen)
  
  mbser <- head(sapply((genmax+1):(length(Svec)), function(x) sum(backwardinc[,x-genmax]*rev(mbgentmp[(x-genmax):x])))+mbinc,-genmax) -
    tail(mfinc, -genmax)
  
  Rc <- R0 * sapply(1:(length(Svec)-genmax), function(x) sum(gen[1:(1+genmax)]*Svec[x:(x+genmax)]/N))
  
  backwardinc2 <- sapply((genmax+1):(length(Svec)), function(x) Rc[x:(x-genmax)]*inc[1:(1+genmax)]*Ivec[x:(x-genmax)]/sum(Rc[x:(x-genmax)]*inc[1:(1+genmax)]*Ivec[x:(x-genmax)]))
  mbinc2 <- apply(backwardinc2, 2, function(x) sum(x*0:genmax*dt)/sum(x))
  
  mfser2 <- head(sapply((genmax+1):(length(Svec)), function(x) sum(backwardinc2[,x-genmax]*rev(mfgen[(x-genmax):x]))),-genmax) +
    tail(mfinc, -genmax) - head(mbinc2, -genmax)
  
  list(
    tvec=head(tvec, -2*genmax),
    cI=tail(head(cumsum(Ivec), -genmax), -genmax),
    Ivec=tail(head(Ivec, -genmax), -genmax),
    Svec=tail(head(Svec, -genmax), -genmax),
    mfgen=tail(mfgen, -genmax),
    mbgen=head(mbgen, -genmax),
    mfinc=tail(mfinc, -genmax),
    mbinc=head(mbinc, -genmax),
    mbinc2=head(mbinc2, -genmax),
    mfser=mfser,
    mfser2=mfser2,
    mbser=mbser,
    Rc=tail(Rc, -genmax),
    gen=gen,
    R0=R0,
    r=r
  )
}

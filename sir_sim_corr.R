source("sir_corr.R")

nsim <- 10

corr <- c(0, 0.2, 0.4, 0.6, 0.8)

simlist_corr <- vector('list', length(corr))

for (j in 1:length(corr)) {
  print(j)
  simlist <- vector('list', nsim)
  
  i <- 1
  
  while (i <= nsim) {
    print(i)
    sir_sim <- sir.full2(size=40000, I0=10, seed=i, rho=corr[j], keep.intrinsic = FALSE)
    
    if (nrow(sir_sim$data) > 100) {
      simlist[[i]] <- sir_sim
      i <- i +1
    }
  }
  
  simlist_corr[[j]] <- simlist
}

save("simlist_corr", file="sir_sim_corr.rda")

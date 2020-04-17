source("sir.R")

nsim <- 10

simlist <- vector('list', nsim)

i <- 1

while (i <= nsim) {
  print(i)
  sir_sim <- sir.full(size=40000, I0=10, seed=i, keep.intrinsic = TRUE)
  
  if (nrow(sir_sim$data) > 100) {
    simlist[[i]] <- sir_sim
    i <- i +1
  }
}

save("simlist", file="sir_sim.rda")

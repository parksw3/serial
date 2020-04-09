source("sir.R")

sir_sim <- sir.full(size=40000, I0=10, seed=123, keep.intrinsic = TRUE)

save("sir_sim", file="sir_sim.rda")

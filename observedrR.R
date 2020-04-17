library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
library(tikzDevice)
source("renewal_det.R")
source("cpalette.R")

nboot <- 200

load("sir_sim.rda")

rr <- renewal_det()
r <- rr$r

tcut <- seq(10, 80, by=2)

serdata0 <- data_frame(
  cohort=sir_sim$t_symptomatic[sir_sim$infected_by],
  tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by],
  tobs=pmax(cohort, cohort+tdiff)
)

R0_obs <- lapply(tcut, function(x) {
  set.seed(101)
  tmp <- serdata0 %>%
    filter(tobs < x)
  
  R0samp <- replicate(nboot, {
    ss <- tmp$tdiff
    ss <- sample(ss, replace=TRUE)
    1/mean(exp(-r*ss), na.rm=TRUE)
  })
  
  data.frame(
    t=x,
    R0=1/mean(exp(-r*tmp$tdiff), na.rm=TRUE),
    lwr=quantile(R0samp, 0.025),
    upr=quantile(R0samp, 0.975)
  )
}) %>%
  bind_rows %>%
  mutate(
    type="Observed"
  )

R0_cohort <- lapply(tcut, function(x) {
  set.seed(101)
  tmp <- serdata0 %>%
    filter(cohort < x)
  
  R0samp <- replicate(nboot, {
    ss <- tmp$tdiff
    ss <- sample(ss, replace=TRUE)
    1/mean(exp(-r*ss), na.rm=TRUE)
  })
  
  data.frame(
    t=x,
    R0=1/mean(exp(-r*tmp$tdiff), na.rm=TRUE),
    lwr=quantile(R0samp, 0.025),
    upr=quantile(R0samp, 0.975)
  )
}) %>%
  bind_rows  %>%
  mutate(
    type="Cohort-averaged"
  )

R0all <- bind_rows(R0_obs, R0_cohort)

g1 <- ggplot(R0all) +
  geom_ribbon(aes(t, ymin=lwr, ymax=upr, fill=type, lty=type, col=type), alpha=0.5) +
  geom_line(aes(t, R0, col=type, lty=type)) +
  geom_hline(yintercept=rr$R0, lty=2) +
  scale_x_continuous("Time (days)", limits=c(0, 80), expand=c(0, 0)) +
  scale_y_continuous("Reproduction number $\\mathcal R$", expand=c(0, 0), limits=c(1.3, 2.6)) +
  scale_color_manual(values=tail(cpalette, 2)) +
  scale_fill_manual(values=tail(cpalette, 2)) +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.15),
    legend.title = element_blank()
  )

gen_obs <- lapply(tcut, function(x) {
  set.seed(101)
  tmp <- serdata0 %>%
    filter(tobs < x)
  
  gensamp <- replicate(nboot, {
    ss <- tmp$tdiff
    ss <- sample(ss, replace=TRUE)
    mean(ss, na.rm=TRUE)
  })
  
  data.frame(
    t=x,
    est=mean(tmp$tdiff, na.rm=TRUE),
    lwr=quantile(gensamp, 0.025),
    upr=quantile(gensamp, 0.975)
  )
}) %>%
  bind_rows %>%
  mutate(
    type="Observed"
  )

plot(gen_obs$est)
lines(gen_obs$lwr)
lines(gen_obs$upr)

tikz(file = "observedrR.tex", width = 6, height = 4, standAlone = T)
plot(g1)
dev.off()
tools::texi2dvi('observedrR.tex', pdf = T, clean = T)

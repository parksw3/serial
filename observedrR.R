library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
library(tikzDevice)
source("renewal_det.R")
source("cpalette.R")

load("sir_sim.rda")

rr <- renewal_det()
r <- rr$r

tcut <- seq(10, 80, by=2)

sir_sim <- lapply(simlist, function(x) {
  data.frame(
    t_infected=x$t_infected,
    t_symptomatic=x$t_symptomatic,
    infected_by=x$infected_by
  )
}) %>%
  bind_rows(.id="sim") %>%
  mutate(
    infected_by=infected_by+(as.numeric(sim)-1)*40000
  )

serdata0 <- data_frame(
  cohort=sir_sim$t_symptomatic[sir_sim$infected_by],
  tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by],
  tobs=pmax(cohort, cohort+tdiff),
  sim=sir_sim$sim
)

ser_obs <- lapply(tcut, function(x) {
  set.seed(101)
  tmp <- serdata0 %>%
    filter(tobs < x)
  
  tmp %>%
    group_by(sim) %>%
    summarize(
      ser=mean(tdiff, na.rm=TRUE)
    ) %>%
    mutate(
      t=x
    )
}) %>%
  bind_rows %>%
  mutate(
    type="Observed"
  )

R0_obs <- lapply(tcut, function(x) {
  set.seed(101)
  tmp <- serdata0 %>%
    filter(tobs < x)
  
  tmp %>%
    group_by(sim) %>%
    summarize(
      R0=1/mean(exp(-r*tdiff), na.rm=TRUE)
    ) %>%
    mutate(
      t=x
    )
}) %>%
  bind_rows %>%
  mutate(
    type="Observed"
  )

ser_cohort <- lapply(tcut, function(x) {
  set.seed(101)
  tmp <- serdata0 %>%
    filter(cohort < x)
  
  tmp %>%
    group_by(sim) %>%
    summarize(
      ser=mean(tdiff, na.rm=TRUE)
    ) %>%
    mutate(
      t=x
    )
}) %>%
  bind_rows %>%
  mutate(
    type="Cohort-averaged"
  )

R0_cohort <- lapply(tcut, function(x) {
  set.seed(101)
  tmp <- serdata0 %>%
    filter(cohort < x)
  
  tmp %>%
    group_by(sim) %>%
    summarize(
      R0=1/mean(exp(-r*tdiff), na.rm=TRUE)
    ) %>%
    mutate(
      t=x
    )
}) %>%
  bind_rows  %>%
  mutate(
    type="Cohort-averaged"
  )

ser_all <- bind_rows(ser_obs, ser_cohort) %>%
  mutate(
    type=factor(type, level=c("Observed", "Cohort-averaged"))
  )

g1 <- ggplot(ser_all) +
  geom_line(aes(t, ser, col=type, group=interaction(sim, type), lty=type)) +
  geom_hline(yintercept=rr$mfser2[1], lty=2) +
  scale_x_continuous("Time (days)", limits=c(0, 80), expand=c(0, 0)) +
  scale_y_continuous("Mean serial interval (days)", expand=c(0, 0), limits=c(2, 7.2)) +
  scale_color_manual(values=tail(cpalette, 2)) +
  scale_fill_manual(values=tail(cpalette, 2)) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.75, 0.2),
    legend.title = element_blank()
  )

R0all <- bind_rows(R0_obs, R0_cohort) %>%
  mutate(
    type=factor(type, level=c("Observed", "Cohort-averaged"))
  )

g2 <- ggplot(R0all) +
  geom_line(aes(t, R0, col=type, group=interaction(sim, type), lty=type)) +
  geom_hline(yintercept=rr$R0, lty=2) +
  scale_x_continuous("Time (days)", limits=c(0, 80), expand=c(0, 0)) +
  scale_y_continuous("Reproduction number $\\mathcal R$", expand=c(0, 0), limits=c(1.3, 3.3)) +
  scale_color_manual(values=tail(cpalette, 2)) +
  scale_fill_manual(values=tail(cpalette, 2)) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

tikz(file = "observedrR.tex", width = 8, height = 3, standAlone = T)
grid.arrange(g1, g2, nrow=1)
dev.off()
tools::texi2dvi('observedrR.tex', pdf = T, clean = T)

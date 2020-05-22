library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size = 16))
library(gridExtra)
library(tikzDevice)
source("rRfun.R")
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
  ) %>%
  group_by(type, t) %>%
  summarize(
    mean=mean(ser),
    lwr=min(ser),
    upr=max(ser)
  )

serialsim <- serialR2(r=r, rho=0) 

g1a <- ggplot(filter(ser_all, type=="Observed")) +
  geom_ribbon(aes(t, ymin=lwr, ymax=upr, fill=type, col=type, lty=type), alpha=0.5) +
  geom_line(aes(t, mean, col=type, lty=type)) +
  geom_hline(yintercept=rr$mfser2[1], lty=2) +
  geom_hline(yintercept=serialsim$mean, lty=1) +
  annotate("text", x=78, y=rr$mfser2[1], label="Initial forward", hjust=1, vjust=-0.3, size=5) +
  annotate("text", x=78, y=serialsim$mean, label="Intrinsic", hjust=1, vjust=-0.3, size=5) +
  scale_x_continuous("Time (days)", limits=c(0, 80), expand=c(0, 0)) +
  scale_y_continuous("Mean serial interval (days)", expand=c(0, 0), limits=c(2, 6.3)) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

g1b <- ggplot(filter(ser_all, type=="Cohort-averaged")) +
  geom_ribbon(aes(t, ymin=lwr, ymax=upr, fill=type, col=type, lty=type), alpha=0.5) +
  geom_line(aes(t, mean, col=type, lty=type)) +
  geom_hline(yintercept=rr$mfser2[1], lty=2) +
  geom_hline(yintercept=serialsim$mean, lty=1) +
  annotate("text", x=78, y=rr$mfser2[1], label="Initial forward", hjust=1, vjust=-0.3, size=5) +
  annotate("text", x=78, y=serialsim$mean, label="Intrinsic", hjust=1, vjust=-0.3, size=5) +
  scale_x_continuous("Time (days)", limits=c(0, 80), expand=c(0, 0)) +
  scale_y_continuous("Mean serial interval (days)", expand=c(0, 0), limits=c(4.5, 7.2)) +
  scale_color_manual(values=tail(cpalette, 1)) +
  scale_fill_manual(values=tail(cpalette, 1)) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

R0all <- bind_rows(R0_obs, R0_cohort) %>%
  mutate(
    type=factor(type, level=c("Observed", "Cohort-averaged"))
  ) %>%
  group_by(type, t) %>%
  summarize(
    mean=mean(R0),
    lwr=min(R0),
    upr=max(R0)
  )

g2a <- ggplot(filter(R0all, type=="Observed")) +
  geom_ribbon(aes(t, ymin=lwr, ymax=upr, fill=type, col=type, lty=type), alpha=0.5) +
  geom_line(aes(t, mean, col=type, lty=type)) +
  geom_hline(yintercept=rr$R0, lty=2) +
  geom_hline(yintercept=serialsim$R, lty=1) +
  annotate("text", x=78, y=rr$R0, label="Initial forward", hjust=1, vjust=-0.3, size=5) +
  annotate("text", x=78, y=serialsim$R, label="Intrinsic", hjust=1, vjust=-0.3, size=5) +
  scale_x_continuous("Time (days)", limits=c(0, 80), expand=c(0, 0)) +
  scale_y_continuous("Basic reproduction number $\\mathcal R_0$", expand=c(0, 0), limits=c(0.9, 2.7),
                     breaks=c(1, 2, 3)) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

g2b <- ggplot(filter(R0all, type=="Cohort-averaged")) +
  geom_ribbon(aes(t, ymin=lwr, ymax=upr, fill=type, col=type, lty=type), alpha=0.5) +
  geom_line(aes(t, mean, col=type, lty=type)) +
  geom_hline(yintercept=rr$R0, lty=2) +
  geom_hline(yintercept=serialsim$R, lty=1) +
  annotate("text", x=78, y=rr$R0, label="Initial forward", hjust=1, vjust=-0.3, size=5) +
  annotate("text", x=78, y=serialsim$R, label="Intrinsic", hjust=1, vjust=-0.3, size=5) +
  scale_x_continuous("Time (days)", limits=c(0, 80), expand=c(0, 0)) +
  scale_y_continuous("Basic reproduction number $\\mathcal R_0$", expand=c(0, 0), limits=c(1.3, 3.3),
                     breaks=c(1, 2, 3)) +
  scale_color_manual(values=tail(cpalette, 1)) +
  scale_fill_manual(values=tail(cpalette, 1)) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

tikz(file = "observedrRa.tex", width = 6.6, height = 3.5, standAlone = T)
grid.arrange(g1a, g2a, nrow=1)
dev.off()
tools::texi2dvi('observedrRa.tex', pdf = T, clean = T)

tikz(file = "observedrRb.tex", width = 6.6, height = 3.5, standAlone = T)
grid.arrange(g1b, g2b, nrow=1)
dev.off()
tools::texi2dvi('observedrRb.tex', pdf = T, clean = T)

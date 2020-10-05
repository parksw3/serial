library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
library(tikzDevice)
source("renewal_det.R")

load("sir_sim.rda")

rr <- renewal_det()

summfun <- function(dd) {
  dd %>%
    filter(!is.na(cohort), !is.na(tdiff), cohort <= 100) %>%
    mutate(
      cc=cut(cohort, tcut)
    ) %>%
    group_by(cc) %>%
    summarize(
        mean=mean(tdiff)
    ) %>%
    ungroup %>%
    mutate(
      cc=as.character(cc),
      cc=(as.numeric(gsub("\\(", "", gsub(",.*", "", cc)))+as.numeric(gsub("]", "", gsub(".*,", "", cc))))/2
    )
}

## model parameters
r <- rr$r

detdata <- data.frame(
  tvec=rr$tvec,
  ci=rr$cI
)

sir_data <- lapply(simlist, "[[", "data") %>%
  bind_rows(.id="sim")

sir_data2 <- sir_data %>%
  mutate(
    day=floor(time)
  ) %>%
  group_by(day, sim) %>%
  summarize(
    cases=tail(infected, 1)-head(infected,1)
  )

sir_data2_time <- sir_data2 %>%
  filter(cases >= 100) %>%
  group_by(sim) %>%
  summarize(
    min=min(day)
  )

sir_data3 <- sir_data2 %>%
  merge(sir_data2_time) %>%
  mutate(
    day=day-min
  )

detdata2 <- detdata %>%
  mutate(
    day=floor(tvec)
  ) %>%
  group_by(day) %>%
  summarize(
    cases=tail(ci, 1)-head(ci,1)
  ) %>%
  mutate(
    min=min(day[cases >= 100])
  ) %>%
  mutate(
    day=day-min
  )

g1 <- ggplot(sir_data3) +
  geom_line(aes(day, cases, col="Stochastic", group=sim)) +
  geom_line(data=detdata2, aes(day, cases, col="Deterministic"), lwd=1) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Daily incidence", expand=c(0, 0), limits=c(1, 2400)) +
  scale_color_manual(values=c(1, "gray")) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.1, 0.7)
  )

tcut <- (-20):20*4

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

incdata0 <- data.frame(
  cohort=sir_sim$t_infected,
  tdiff=sir_sim$t_symptomatic-sir_sim$t_infected,
  sim=sir_sim$sim
) %>%
  merge(sir_data2_time) %>%
  mutate(
    cohort=cohort-min
  ) %>%
  summfun()

incdet <- data.frame(
  tvec=rr$tvec-detdata2$min[1],
  mf=rr$mfinc
)

g2 <- ggplot(incdata0) +
  geom_line(data=incdet, aes(tvec, mf), col="#81b214", lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#81b214", size=2) +
  geom_hline(yintercept=incdet$mf[1], lty=2) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7), oob=scales::squish) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("B. Forward\nIncubation period") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gendata0 <- data.frame(
  cohort=sir_sim$t_infected[sir_sim$infected_by],
  tdiff=sir_sim$t_infected-sir_sim$t_infected[sir_sim$infected_by],
  sim=sir_sim$sim) %>%
  merge(sir_data2_time) %>%
  mutate(
    cohort=cohort-min
  ) %>%
  summfun()

gendet <- data.frame(
  tvec=rr$tvec-detdata2$min[1],
  mf=rr$mfgen
)

g3 <- ggplot(gendata0) +
  geom_line(data=gendet, aes(tvec, mf), col="#81b214", lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#81b214", size=2) +
  geom_hline(yintercept=gendet$mf[1], lty=2) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7), breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("C. Forward\nGeneration interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

serdata0 <- data.frame(
  cohort=sir_sim$t_symptomatic[sir_sim$infected_by],
  tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by],
  sim=sir_sim$sim) %>%
  merge(sir_data2_time) %>%
  mutate(
    cohort=cohort-min
  ) %>%
  summfun()

serdet <- data.frame(
  tvec=rr$tvec-detdata2$min[1],
  mf=rr$mfser2
)

g4 <- ggplot(serdata0) +
  geom_line(data=serdet, aes(tvec, mf), col="#81b214", lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#81b214", size=2) +
  geom_hline(yintercept=serdet$mf[1], lty=2) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("D. Forward\nSerial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

incdata1 <- data.frame(
  cohort=sir_sim$t_symptomatic,
  tdiff=sir_sim$t_symptomatic-sir_sim$t_infected,
  sim=sir_sim$sim) %>%
  merge(sir_data2_time) %>%
  mutate(
    cohort=cohort-min
  ) %>%
  summfun()

incdet1 <- data.frame(
  tvec=rr$tvec-detdata2$min[1],
  mb=rr$mbinc
)

g5 <- ggplot(incdata1) +
  geom_line(data=incdet1, aes(tvec, mb), col="#07689f", lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#07689f", size=2) +
  geom_hline(yintercept=incdet$mf[1], lty=2) +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6),
                     breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("E. Backward\nIncubation period") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gendata1 <- data.frame(
  cohort=sir_sim$t_infected,
  tdiff=sir_sim$t_infected-sir_sim$t_infected[sir_sim$infected_by],
  sim=sir_sim$sim) %>%
  merge(sir_data2_time) %>%
  mutate(
    cohort=cohort-min
  ) %>%
  summfun()

gendet1 <- data.frame(
  tvec=rr$tvec-detdata2$min[1],
  mb=rr$mbgen
)

g6 <- ggplot(gendata1) +
  geom_line(data=gendet1, aes(tvec, mb), col="#07689f", lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#07689f", size=2) +
  geom_hline(yintercept=gendet$mf[1], lty=2) +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6), breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("F. Backward\nGeneration interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

serdata1 <- data.frame(
  cohort=sir_sim$t_symptomatic,
  tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by],
  sim=sir_sim$sim) %>%
  merge(sir_data2_time) %>%
  mutate(
    cohort=cohort-min
  ) %>%
  summfun()

serdet1 <- data.frame(
  tvec=rr$tvec-detdata2$min[1],
  mb=rr$mbser
)

g7 <- ggplot(serdata1) +
  geom_line(data=serdet1, aes(tvec, mb), col="#07689f", lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#07689f", size=2) +
  geom_hline(yintercept=serdet$mf[1], lty=2) +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6),
                     breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("G. Backward\nSerial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gtot <- arrangeGrob(g1, g2, g3, g4, g5, g6, g7, layout_matrix = matrix(c(1, 1, 1, 2, 3, 4, 5, 6, 7), nrow=3, byrow=TRUE),
                    heights=c(0.8, 1, 1))

tikz(file = "forward.tex", width = 8, height = 7, standAlone = T)
plot(gtot)
dev.off()
tools::texi2dvi('forward.tex', pdf = T, clean = T)

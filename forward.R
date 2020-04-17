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
    filter(!is.na(cohort), !is.na(tdiff), cohort > 0, cohort <= 100) %>%
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
    time=ceiling(time*5)/5
  ) %>%
  group_by(sim, time) %>%
  filter(infected==max(infected))

g1 <- ggplot(sir_data2) +
  geom_line(aes(time, infected, col="Stochastic", group=sim)) +
  geom_line(data=detdata, aes(tvec, ci, col="Deterministic"), lwd=1) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_log10("Cumulative incidence", expand=c(0, 0), limits=c(10, 49999)) +
  scale_color_manual(values=c(1, "#D55E00")) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.1, 0.7)
  )

tcut <- 0:40*2

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
  tdiff=sir_sim$t_symptomatic-sir_sim$t_infected
) %>%
  summfun()

incdet <- data.frame(
  tvec=rr$tvec,
  mf=rr$mfinc
)

g2 <- ggplot(incdata0) +
  geom_line(data=incdet, aes(tvec, mf), lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#D55E00", size=2) +
  geom_hline(yintercept=incdet$mf[1], lty=2) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("B. Incubation period") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gendata0 <- data.frame(
  cohort=sir_sim$t_infected[sir_sim$infected_by],
  tdiff=sir_sim$t_infected-sir_sim$t_infected[sir_sim$infected_by]
) %>%
  summfun()

gendet <- data.frame(
  tvec=rr$tvec,
  mf=rr$mfgen
)

g3 <- ggplot(gendata0) +
  geom_line(data=gendet, aes(tvec, mf), lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#D55E00", size=2) +
  geom_hline(yintercept=gendet$mf[1], lty=2) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7), breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("C. Generation interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

serdata0 <- data.frame(
  cohort=sir_sim$t_symptomatic[sir_sim$infected_by],
  tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by]
) %>%
  summfun()

serdet <- data.frame(
  tvec=rr$tvec,
  mf=rr$mfser2
)

g4 <- ggplot(serdata0) +
  geom_line(data=serdet, aes(tvec, mf), lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#D55E00", size=2) +
  geom_hline(yintercept=serdet$mf[1], lty=2) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("D. Serial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

incdata1 <- data.frame(
  cohort=sir_sim$t_symptomatic,
  tdiff=sir_sim$t_symptomatic-sir_sim$t_infected
) %>%
  summfun()

incdet1 <- data.frame(
  tvec=rr$tvec,
  mb=rr$mbinc
)

g5 <- ggplot(incdata1) +
  geom_line(data=incdet1, aes(tvec, mb), lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#D55E00", size=2) +
  geom_hline(yintercept=incdet$mf[1], lty=2) +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6),
                     breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("E. Incubation period") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gendata1 <- data.frame(
  cohort=sir_sim$t_infected,
  tdiff=sir_sim$t_infected-sir_sim$t_infected[sir_sim$infected_by]
) %>%
  summfun()

gendet1 <- data.frame(
  tvec=rr$tvec,
  mb=rr$mbgen
)

g6 <- ggplot(gendata1) +
  geom_line(data=gendet1, aes(tvec, mb), lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#D55E00", size=2) +
  geom_hline(yintercept=gendet$mf[1], lty=2) +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6), breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("F. Generation interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

serdata1 <- data.frame(
  cohort=sir_sim$t_symptomatic,
  tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by]
) %>%
  summfun()

serdet1 <- data.frame(
  tvec=rr$tvec,
  mb=rr$mbser
)

g7 <- ggplot(serdata1) +
  geom_line(data=serdet1, aes(tvec, mb), lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#D55E00", size=2) +
  geom_hline(yintercept=serdet$mf[1], lty=2) +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6),
                     breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("G. Serial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gtot <- arrangeGrob(g1, g2, g3, g4, g5, g6, g7, layout_matrix = matrix(c(1, 1, 1, 2, 3, 4, 5, 6, 7), nrow=3, byrow=TRUE),
                    heights=c(0.8, 1, 1))

tikz(file = "forward.tex", width = 8, height = 8, standAlone = T)
plot(gtot)
dev.off()
tools::texi2dvi('forward.tex', pdf = T, clean = T)

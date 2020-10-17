library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
library(tikzDevice)

load("sir_sim_corr.rda")

corr_data <- data.frame(
  corr=1:4,
  type=paste0("$\\rho=", c(0, 0.25, 0.5, 0.75), "$")
) %>%
  mutate(
    type=factor(type, levels=type)
  )

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

sir_data <- lapply(simlist_corr, function(x) {
  lapply(x, "[[", "data") %>%
    bind_rows(.id="sim")
}) %>%
  bind_rows(.id='corr')

sir_data2 <- sir_data %>%
  mutate(
    day=floor(time)
  ) %>%
  group_by(corr, day, sim) %>%
  summarize(
    cases=tail(infected, 1)-head(infected,1)
  )

sir_data2_time <- sir_data2 %>%
  filter(cases >= 100) %>%
  group_by(corr, sim) %>%
  summarize(
    min=min(day)
  )

sir_data3 <- sir_data2 %>%
  merge(sir_data2_time) %>%
  mutate(
    day=day-min
  ) %>%
  merge(corr_data)

g1 <- ggplot(sir_data3) +
  geom_line(aes(day, cases, col=type, group=interaction(type, sim))) +
  # geom_line(data=detdata2, aes(day, cases, col="Deterministic"), lwd=1) +
  scale_x_continuous("Time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Daily incidence", expand=c(0, 0), limits=c(1, 2400)) +
  scale_color_viridis_d() +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.08, 0.55),
    legend.background = element_rect(fill="transparent", color=NA),
    legend.box.background = element_rect(fill="transparent", color=NA)
  )

tcut <- (-20):20*4

sir_sim_list <- lapply(simlist_corr, function(ss) {
  lapply(ss, function(x) {
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
})

incdata0 <- lapply(sir_sim_list, function(sir_sim) {
  data.frame(
    cohort=sir_sim$t_infected,
    tdiff=sir_sim$t_symptomatic-sir_sim$t_infected,
    sim=sir_sim$sim
  ) %>%
    merge(sir_data2_time) %>%
    mutate(
      cohort=cohort-min
    ) %>%
    summfun()
}) %>%
  bind_rows(.id="corr") %>%
  merge(corr_data)

g2 <- ggplot(incdata0) +
  # geom_line(data=incdet, aes(tvec, mf), lwd=1) +
  geom_point(aes(cc, mean, col=type), shape=1, size=2) +
  geom_smooth(aes(cc, mean, col=type), se=FALSE) +
  # geom_hline(yintercept=incdet$mf[1], lty=2) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7), oob=scales::squish) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("B. Incubation period") +
  scale_color_viridis_d() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gendata0 <- lapply(sir_sim_list, function(sir_sim) {
  data.frame(
    cohort=sir_sim$t_infected[sir_sim$infected_by],
    tdiff=sir_sim$t_infected-sir_sim$t_infected[sir_sim$infected_by],
    sim=sir_sim$sim
  ) %>%
    merge(sir_data2_time) %>%
    mutate(
      cohort=cohort-min
    ) %>%
    summfun()
}) %>%
  bind_rows(.id="corr") %>%
  merge(corr_data)

g3 <- ggplot(gendata0) +
  geom_point(aes(cc, mean, col=type), shape=1, size=2) +
  geom_smooth(aes(cc, mean, col=type), se=FALSE) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7), breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("C. Generation interval") +
  scale_color_viridis_d() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

serdata0 <- lapply(sir_sim_list, function(sir_sim) {
  data.frame(
    cohort=sir_sim$t_symptomatic[sir_sim$infected_by],
    tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by],
    sim=sir_sim$sim
  ) %>%
    merge(sir_data2_time) %>%
    mutate(
      cohort=cohort-min
    ) %>%
    summfun()
}) %>%
  bind_rows(.id="corr") %>%
  merge(corr_data)

g4 <- ggplot(serdata0) +
  geom_point(aes(cc, mean, col=type), shape=1, size=2) +
  geom_smooth(aes(cc, mean, col=type), se=FALSE) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  scale_color_viridis_d() +
  ggtitle("D. Serial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

incdata1 <- lapply(sir_sim_list, function(sir_sim) {
  data.frame(
    cohort=sir_sim$t_symptomatic,
    tdiff=sir_sim$t_symptomatic-sir_sim$t_infected,
    sim=sir_sim$sim
  ) %>%
    merge(sir_data2_time) %>%
    mutate(
      cohort=cohort-min
    ) %>%
    summfun()
}) %>%
  bind_rows(.id="corr") %>%
  merge(corr_data)

g5 <- ggplot(incdata1) +
  geom_point(aes(cc, mean, col=type), shape=1, size=2) +
  geom_smooth(aes(cc, mean, col=type), se=FALSE) +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6),
                     breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  scale_color_viridis_d() +
  ggtitle("E. Incubation period") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gendata1 <- lapply(sir_sim_list, function(sir_sim) {
  data.frame(
    cohort=sir_sim$t_infected,
    tdiff=sir_sim$t_infected-sir_sim$t_infected[sir_sim$infected_by],
    sim=sir_sim$sim
  ) %>%
    merge(sir_data2_time) %>%
    mutate(
      cohort=cohort-min
    ) %>%
    summfun()
}) %>%
  bind_rows(.id="corr") %>%
  merge(corr_data)

g6 <- ggplot(gendata1) +
  geom_point(aes(cc, mean, col=type), shape=1, size=2) +
  geom_smooth(aes(cc, mean, col=type), se=FALSE) +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6), breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  scale_color_viridis_d() +
  ggtitle("F. Generation interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

serdata1 <- lapply(sir_sim_list, function(sir_sim) {
  data.frame(
    cohort=sir_sim$t_symptomatic,
    tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by],
    sim=sir_sim$sim
  ) %>%
    merge(sir_data2_time) %>%
    mutate(
      cohort=cohort-min
    ) %>%
    summfun()
}) %>%
  bind_rows(.id="corr") %>%
  merge(corr_data)

g7 <- ggplot(serdata1) +
  geom_point(aes(cc, mean, col=type), shape=1, size=2) +
  geom_smooth(aes(cc, mean, col=type), se=FALSE) +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(-10, 52)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6),
                     breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  scale_color_viridis_d() +
  ggtitle("G. Serial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

gtot <- arrangeGrob(g1, g2, g3, g4, g5, g6, g7, layout_matrix = matrix(c(1, 1, 1, 2, 3, 4, 5, 6, 7), nrow=3, byrow=TRUE),
                    heights=c(0.8, 1, 1))

tikz(file = "forward_corr.tex", width = 8, height = 8, standAlone = T)
plot(gtot)
dev.off()
tools::texi2dvi('forward_corr.tex', pdf = T, clean = T)

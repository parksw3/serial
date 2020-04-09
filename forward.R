library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
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
meanlog <- 1.621
sdlog <- 0.418
scale <- 5.5
shape <- 4.1
r <- rr$r

detdata <- data.frame(
  tvec=rr$tvec,
  ci=rr$cI
)

g1 <- ggplot(sir_sim$data) +
  geom_line(data=detdata, aes(tvec, ci)) +
  geom_line(aes(time, infected), col=2) +
  scale_x_continuous("Time", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_log10("Cumulative incidence", expand=c(0, 0), limits=c(10, 49999)) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

tcut <- 0:20*4

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
  geom_line(data=incdet, aes(tvec, mf)) +
  geom_point(aes(cc, mean), shape=1, col="red") +
  scale_x_continuous("Cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Forward incubation period (days)", expand=c(0, 0), limits=c(0, 7)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
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
  geom_line(data=gendet, aes(tvec, mf)) +
  geom_point(aes(cc, mean), shape=1, col="red") +
  scale_x_continuous("Cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Forward generation interval (days)", expand=c(0, 0), limits=c(0, 6.2), breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("C") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none"
  )

serdata0 <- data.frame(
  cohort=sir_sim$t_symptomatic[sir_sim$infected_by],
  tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by]
) %>%
  summfun()

serdet_naive <- data.frame(
  tvec=rr$tvec,
  mf=-rr$mbinc+rr$mfgen+rr$mfinc
)

g4 <- ggplot(serdata0) +
  geom_line(data=serdet_naive, aes(tvec, mf)) +
  geom_point(aes(cc, mean), shape=1, col="red") +
  scale_x_continuous("Cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Forward serial interval (days)", expand=c(0, 0), limits=c(0, 7)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("D") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
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
  geom_line(data=incdet1, aes(tvec, mb)) +
  geom_point(aes(cc, mean), shape=1, col="red") +
  scale_x_continuous("Cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Backward incubation period (days)", expand=c(0, 0), limits=c(0, 8.6)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("E") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
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
  geom_line(data=gendet1, aes(tvec, mb)) +
  geom_point(aes(cc, mean), shape=1, col="red") +
  scale_x_continuous("Cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Backward generation interval (days)", expand=c(0, 0), limits=c(0, 6.2), breaks=0:4*2) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none"
  )

serdata1 <- data.frame(
  cohort=sir_sim$t_symptomatic,
  tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by]
) %>%
  summfun()

serdet_naive1 <- data.frame(
  tvec=rr$tvec,
  mf=-rr$mfinc+rr$mbgen+rr$mbinc
)

g7 <- ggplot(serdata1) +
  geom_line(data=serdet_naive1, aes(tvec, mf)) +
  geom_point(aes(cc, mean), shape=1, col="red") +
  scale_x_continuous("Cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Backward serial interval (days)", expand=c(0, 0), limits=c(0, 8.2)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("G") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none"
  )

gtot <- arrangeGrob(g1, g2, g3, g4, g5, g6, g7, layout_matrix = matrix(c(1, 1, 1, 2, 3, 4, 5, 6, 7), nrow=3, byrow=TRUE),
                    heights=c(0.8, 1, 1))

ggsave("forward.pdf", gtot, width=8, height=8)

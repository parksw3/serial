library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
library(tikzDevice)
source("renewal_det.R")

load("sir_sim.rda")

rr <- renewal_det()

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

serdata0 <- data.frame(
  cohort=sir_sim$t_symptomatic[sir_sim$infected_by],
  tdiff=sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by]
) %>%
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

serdet <- data.frame(
  tvec=rr$tvec,
  mf=rr$mfser2,
  type="Accounting for changes in $\\mathcal R_c$"
)

serdet2 <- data.frame(
  tvec=rr$tvec,
  mf=rr$mfser,
  type="Not accounting for changes in $\\mathcal R_c$"
)

serdet_all <- bind_rows(serdet, serdet2)

g1 <- ggplot(serdata0) +
  geom_line(data=serdet_all, aes(tvec, mf, lty=type), lwd=1) +
  geom_point(aes(cc, mean), shape=1, col="#D55E00", size=2) +
  # geom_hline(yintercept=serdet$mf[1], lty=2) +
  scale_x_continuous("Primary cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(0, 7),
                     breaks=c(0.0, 1.5, 3.0, 4.5, 6.0)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("A. Serial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.35, 0.15),
    legend.title = element_blank()
  )

incdata <- data.frame(
  cohort=sir_sim$t_symptomatic[unique(sir_sim$infected_by)],
  tdiff=sir_sim$t_symptomatic[unique(sir_sim$infected_by)]-sir_sim$t_infected[unique(sir_sim$infected_by)]
) %>%
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

incdet <- data.frame(
  tvec=rr$tvec,
  mf=rr$mbinc,
  type="All symptomatic cases"
)

incdet2 <- data.frame(
  tvec=rr$tvec,
  mf=rr$mbinc2,
  type="Symptomatic infectors"
)

incdet_all <- bind_rows(incdet, incdet2) %>%
  mutate(
    type=factor(type, levels=c("Symptomatic infectors", "All symptomatic cases"))
  )

g2 <- ggplot(incdata) +
  geom_line(data=incdet_all, aes(tvec, mf, lty=type), lwd=1) +
  geom_point(aes(cc, mean), shape=2, size=2, col="#D55E00") +
  scale_x_continuous("Secondary cohort time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(0, 9.6)) +
  scale_linetype_manual(values=c(1, 3)) +
  ggtitle("B. Incubation period") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.3, 0.15),
    legend.title = element_blank()
  )

tt <- as.data.frame(table(sir_sim$infected_by))

tmp <- data.frame(
  N=1:length(sir_sim$t_infected),
  offspring=0
)

tmp$offspring[match(tt$Var1[-153138], tmp$N)] <- tt$Freq[-153138]
tmp$offspring[tmp$N==3e5] <- tt$Freq[153138]

cordat <- data.frame(
  cohort=sir_sim$t_symptomatic,
  tdiff=sir_sim$t_symptomatic-sir_sim$t_infected,
  offspring=tmp$offspring
) %>%
  filter(!is.na(cohort), !is.na(tdiff), cohort > 0, cohort <= 80) %>%
  mutate(
    cc=cut(cohort, tcut)
  ) %>%
  group_by(cc) %>%
  summarize(
    cor=cor(tdiff, offspring),
    lwr=cor.test(tdiff, offspring)[[9]][1],
    upr=cor.test(tdiff, offspring)[[9]][2]
  ) %>%
  ungroup %>%
  mutate(
    cc=as.character(cc),
    cc=(as.numeric(gsub("\\(", "", gsub(",.*", "", cc)))+as.numeric(gsub("]", "", gsub(".*,", "", cc))))/2
  )

cordat2 <- data.frame(
  cohort=sir_sim$t_infected,
  tdiff=sir_sim$t_symptomatic-sir_sim$t_infected,
  offspring=tmp$offspring
) %>%
  filter(!is.na(cohort), !is.na(tdiff), cohort > 2, cohort <= 78) %>%
  mutate(
    cc=cut(cohort, tcut)
  ) %>%
  group_by(cc) %>%
  summarize(
    cor=cor(tdiff, offspring),
    lwr=cor.test(tdiff, offspring)[[9]][1],
    upr=cor.test(tdiff, offspring)[[9]][2]
  ) %>%
  ungroup %>%
  mutate(
    cc=as.character(cc),
    cc=(as.numeric(gsub("\\(", "", gsub(",.*", "", cc)))+as.numeric(gsub("]", "", gsub(".*,", "", cc))))/2
  )

g3 <- ggplot(cordat) +
  geom_hline(yintercept = 0, lty=2) +
  geom_ribbon(aes(cc, ymin=lwr, ymax=upr), alpha=0.2, fill="#D55E00") +
  geom_line(aes(cc, cor), col="#D55E00") +
  scale_x_continuous("Symptom onset time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Correlation coefficient", expand=c(0, 0), limits=c(-1, 1)) +
  ggtitle("D. Backward correlation") +
  theme(
    panel.grid = element_blank()
  )

g4 <- ggplot(cordat2) +
  geom_hline(yintercept = 0, lty=2) +
  geom_ribbon(aes(cc, ymin=lwr, ymax=upr), alpha=0.2, fill="#D55E00") +
  geom_line(aes(cc, cor), col="#D55E00") +
  scale_x_continuous("Infection time (days)", expand=c(0, 0), limits=c(0, 82)) +
  scale_y_continuous("Correlation coefficient", expand=c(0, 0), limits=c(-1, 1)) +
  ggtitle("C. Forward correlation") +
  theme(
    panel.grid = element_blank()
  )

tikz(file = "forward_tease.tex", width = 8, height = 3, standAlone = T)
grid.arrange(g2 +
               ggtitle("A. Incubation period"),
             g3 +
               ggtitle("B. Backward correlation") , nrow=1)
dev.off()
tools::texi2dvi('forward_tease.tex', pdf = T, clean = T)

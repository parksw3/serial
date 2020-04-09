library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
source("renewal_det.R")

load("sir_sim.rda")

rr <- renewal_det(tmax=110)

plot(sir_sim$data, type="l", log="y")

lines(rr$tvec, cumsum(rr$Ivec), col=2)

## model parameters
meanlog <- 1.621
sdlog <- 0.418
scale <- 5.5
shape <- 4.1
r <- 1/5.5

set.seed(101)
inc <- rlnorm(50000, meanlog, sdlog)
minc <- mean(inc)
winc <- weighted.mean(inc, exp(-r*inc))

gen <- rweibull(50000, shape, scale)
mgen <- mean(gen)
wgen <- weighted.mean(gen, exp(-r*gen))

serforward <- gen+rlnorm(50000, meanlog, sdlog)
ser <- -inc+serforward
mser <- mean(ser)
ser2 <- -sample(inc, prob=exp(-r*inc), replace=TRUE)+serforward
ww <- exp(-r*ser2)
ww[ser2 < 0] <- 1
wser <- weighted.mean(ser2, ww)

g1 <- ggplot(sir_sim$data) +
  stat_function(fun=function(x)15*exp(1/5.5*x), lty=2) +
  geom_line(aes(time, infected)) +
  scale_x_continuous("Time", expand=c(0, 0), limits=c(0, 80)) +
  scale_y_log10("Cumulative incidence", expand=c(0, 0), limits=c(10, 49999)) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

tcut <- 10:32*2
icut <- -80:80/2

incdata0 <- data.frame(
  cohort=sir_sim$t_infected,
  tdiff=sir_sim$t_symptomatic-sir_sim$t_infected
) %>%
  filter(!is.na(cohort), !is.na(tdiff), cohort > 20, cohort <= 64) %>%
  mutate(
    cc=cut(cohort, tcut),
    tt=cut(tdiff, icut)
  ) %>%
  group_by(cc, tt) %>%
  summarize(
    size=length(tt)
  ) %>%
  mutate(
    prop=size/sum(size)
  ) %>%
  ungroup %>%
  mutate(
    cc=as.character(cc),
    cc=(as.numeric(gsub("\\(", "", gsub(",.*", "", cc)))+as.numeric(gsub("]", "", gsub(".*,", "", cc))))/2,
    tt=as.character(tt),
    tt=(as.numeric(gsub("\\(", "", gsub(",.*", "", tt)))+as.numeric(gsub("]", "", gsub(".*,", "", tt))))/2
  )

incdata_summ <- incdata0 %>%
  group_by(cc) %>%
  summarize(
    mean=weighted.mean(tt, prop)
  )

g2 <- ggplot(incdata0) +
  geom_tile(aes(cc, tt, fill=prop)) +
  geom_point(data=incdata_summ, aes(cc, mean), shape=1, col="red") +
  geom_hline(yintercept=minc, col="red") +
  scale_x_continuous("Cohort time (days)", expand=c(0, 0)) +
  scale_y_continuous("Incubation period (days)", expand=c(0, 0), limits=c(0, 16.5)) +
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
  filter(!is.na(cohort), !is.na(tdiff), cohort > 20, cohort <= 64) %>%
  mutate(
    cc=cut(cohort, tcut),
    tt=cut(tdiff, icut)
  ) %>%
  group_by(cc, tt) %>%
  summarize(
    size=length(tt)
  ) %>%
  mutate(
    prop=size/sum(size)
  ) %>%
  ungroup %>%
  mutate(
    cc=as.character(cc),
    cc=(as.numeric(gsub("\\(", "", gsub(",.*", "", cc)))+as.numeric(gsub("]", "", gsub(".*,", "", cc))))/2,
    tt=as.character(tt),
    tt=(as.numeric(gsub("\\(", "", gsub(",.*", "", tt)))+as.numeric(gsub("]", "", gsub(".*,", "", tt))))/2
  )

gendata_summ <- gendata0 %>%
  group_by(cc) %>%
  summarize(
    mean=weighted.mean(tt, prop)
  )

g3 <- ggplot(gendata0) +
  geom_tile(aes(cc, tt, fill=prop)) +
  geom_point(data=gendata_summ, aes(cc, mean), shape=1, col="red") +
  geom_hline(yintercept=mgen, col="red") +
  scale_x_continuous("Cohort time (days)", expand=c(0, 0)) +
  scale_y_continuous("Generation interval (days)", expand=c(0, 0), limits=c(0, 9.2), breaks=0:4*2) +
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
  filter(!is.na(cohort), !is.na(tdiff), cohort > 20, cohort <= 64) %>%
  mutate(
    cc=cut(cohort, tcut),
    tt=cut(tdiff, icut)
  ) %>%
  group_by(cc, tt) %>%
  summarize(
    size=length(tt)
  ) %>%
  mutate(
    prop=size/sum(size)
  ) %>%
  ungroup %>%
  mutate(
    cc=as.character(cc),
    cc=(as.numeric(gsub("\\(", "", gsub(",.*", "", cc)))+as.numeric(gsub("]", "", gsub(".*,", "", cc))))/2,
    tt=as.character(tt),
    tt=(as.numeric(gsub("\\(", "", gsub(",.*", "", tt)))+as.numeric(gsub("]", "", gsub(".*,", "", tt))))/2
  )

serdata_summ <- serdata0 %>%
  group_by(cc) %>%
  summarize(
    mean=weighted.mean(tt, prop)
  )

g4 <- ggplot(serdata0) +
  geom_tile(aes(cc, tt, fill=prop)) +
  geom_hline(yintercept=mean(ser2), col="red") +
  geom_point(data=serdata_summ, aes(cc, mean), shape=1, col="red") +
  scale_x_continuous("Cohort time (days)", expand=c(0, 0)) +
  scale_y_continuous("Serial interval (days)", expand=c(0, 0), limits=c(NA, 15)) +
  scale_fill_gradientn(colors=c("white", "black")) +
  ggtitle("D") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.position = "none"
  )

incdata <- data_frame(
  tmeasure=sir_sim$t_symptomatic[order(sir_sim$t_symptomatic)],
  tdiff=(sir_sim$t_symptomatic-sir_sim$t_infected)[order(sir_sim$t_symptomatic)],
  keep=!is.na(tdiff)
) %>%
  filter(keep) %>%
  mutate(
    csum=cumsum(tdiff),
    csum2=cumsum(tdiff^2),
    cmean=csum/1:n(),
    csd=sqrt((csum2-csum^2/1:n())/(1:n()-1)),
    cse=csd/sqrt(1:n()),
    lwr=cmean-2*cse,
    upr=cmean+2*cse
  ) %>%
  filter(tmeasure > 20, tmeasure < 64)

gendata <- data_frame(
  tmeasure=sir_sim$t_infected[order(sir_sim$t_infected)],
  tdiff=(sir_sim$t_infected-sir_sim$t_infected[sir_sim$infected_by])[order(sir_sim$t_symptomatic)],
  keep=!is.na(tdiff)
) %>%
  filter(keep) %>%
  mutate(
    csum=cumsum(tdiff),
    csum2=cumsum(tdiff^2),
    cmean=csum/1:n(),
    csd=sqrt((csum2-csum^2/1:n())/(1:n()-1)),
    cse=csd/sqrt(1:n()),
    lwr=cmean-2*cse,
    upr=cmean+2*cse
  ) %>%
  filter(tmeasure > 20, tmeasure < 64)

serdata <- data_frame(
  tmeasure=pmax(sir_sim$t_symptomatic, sir_sim$t_symptomatic[sir_sim$infected_by]),
  tdiff=(sir_sim$t_symptomatic-sir_sim$t_symptomatic[sir_sim$infected_by]),
  keep=!is.na(tdiff)
) %>%
  filter(keep) %>%
  arrange(tmeasure) %>%
  mutate(
    csum=cumsum(tdiff),
    csum2=cumsum(tdiff^2),
    cmean=csum/1:n(),
    csd=sqrt((csum2-csum^2/1:n())/(1:n()-1)),
    cse=csd/sqrt(1:n()),
    lwr=cmean-2*cse,
    upr=cmean+2*cse
  ) %>%
  filter(tmeasure > 20, tmeasure < 64)

g5 <- ggplot(incdata) +
  geom_hline(yintercept=minc, col='gray', lwd=2) +
  geom_hline(yintercept=winc, col='gray', lty=2, lwd=2) +
  geom_ribbon(aes(tmeasure, ymin=lwr, ymax=upr), col=1, fill=NA, lty=2) +
  geom_line(aes(tmeasure, cmean), lwd=1) +
  scale_x_continuous("Time of measurement (days)", expand=c(0, 0), limits=c(20, 64)) +
  scale_y_continuous("Mean incubation period (days)", expand=c(0, 0), limits=c(4, minc+0.2)) +
  ggtitle("E") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

g6 <- ggplot(gendata) +
  geom_hline(yintercept=mgen, col='gray', lwd=2) +
  geom_hline(yintercept=wgen, col='gray', lwd=2, lty=2) +
  geom_line(aes(tmeasure, cmean), lwd=1) +
  geom_ribbon(aes(tmeasure, ymin=lwr, ymax=upr), col=1, fill=NA, lty=2) +
  scale_x_continuous("Time of measurement (days)", expand=c(0, 0), limits=c(20, 64)) +
  scale_y_continuous("Mean generation interval (days)", expand=c(0, 0), limits=c(4, mgen+0.2)) +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

g7 <- ggplot(serdata) +
  geom_hline(yintercept=mgen, col='gray', lwd=2) +
  geom_hline(yintercept=wser, col='gray', lwd=2, lty=2) +
  geom_line(aes(tmeasure, cmean), lwd=1) +
  geom_ribbon(aes(tmeasure, ymin=lwr, ymax=upr), col=1, fill=NA, lty=2) +
  scale_x_continuous("Time of measurement (days)", expand=c(0, 0), limits=c(20, 64)) +
  scale_y_continuous("Mean serial interval (days)", expand=c(0, 0), limits=c(3, mser+0.2)) +
  ggtitle("G") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line()
  )

gtot <- arrangeGrob(g1, g2, g3, g4, g5, g6, g7, layout_matrix = matrix(c(1, 1, 1, 2, 3, 4, 5, 6, 7), nrow=3, byrow=TRUE))

ggsave("figure_sir.pdf", gtot, width=10, height=8)

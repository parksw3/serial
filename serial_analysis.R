library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(gridExtra)
library(tikzDevice)
source("cpalette.R")

nboot <- 200

r1 <- log(2)/8
r2 <- log(2)/6

rr <- read_xlsx("Table S5.xlsx", skip=1) %>%
  mutate(
    cohort=`Index - symptom onset date`,
    serial=`Seconday - symptom onset date`- `Index - symptom onset date`
  )

rr_summ <- rr %>%
  group_by(cohort, serial) %>%
  summarize(
    size=length(serial)
  )

rr_summ2 <- rr %>%
  group_by(cohort) %>%
  summarize(
    mean=mean(serial)
  )

tcut <- seq(0, 29, by=1)

serdata1 <- lapply(tcut, function(x) {
  set.seed(123)
  tmp <- rr %>%
    filter(cohort <= x)
  
  R0samp <- replicate(nboot, {
    ss <- tmp$serial
    ss <- sample(ss, replace=TRUE)
    1/mean(exp(-r1*ss), na.rm=TRUE)
  })
  
  data.frame(
    t=x,
    R0=1/mean(exp(-r1*tmp$serial), na.rm=TRUE),
    lwr=quantile(R0samp, 0.025),
    upr=quantile(R0samp, 0.975)
  )
}) %>%
  bind_rows  %>%
  mutate(
    type="8 day doubling period"
  )

serdata2 <- lapply(tcut, function(x) {
  set.seed(123)
  tmp <- rr %>%
    filter(cohort <= x)
  
  R0samp <- replicate(nboot, {
    ss <- tmp$serial
    ss <- sample(ss, replace=TRUE)
    1/mean(exp(-r2*ss), na.rm=TRUE)
  })
  
  data.frame(
    t=x,
    R0=1/mean(exp(-r2*tmp$serial), na.rm=TRUE),
    lwr=quantile(R0samp, 0.025),
    upr=quantile(R0samp, 0.975)
  )
}) %>%
  bind_rows  %>%
  mutate(
    type="6 day doubling period"
  )

serall <- bind_rows(serdata1, serdata2)

g1 <- ggplot(rr_summ) +
  geom_boxplot(aes(cohort, serial, group=cohort)) +
  # geom_point(data=rr_summ2, aes(cohort, mean), col=cpalette[2]) +
  # geom_line(data=rr_summ2, aes(cohort, mean), col=cpalette[2], lty=2) +
  scale_x_continuous("Cohort time (days)", limits=c(-4, 30), expand=c(0, 0)) +
  scale_y_continuous("Forward serial intervals (days)") +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g2 <- ggplot(serall) +
  geom_ribbon(aes(t, ymin=lwr, ymax=upr, fill=type, lty=type, col=type), alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_line(aes(t, R0, col=type, lty=type)) +
  scale_x_continuous("Cohort time (days)", limits=c(-4, 30), expand=c(0, 0)) +
  scale_y_continuous("Reproduction number $\\mathcal R$") +
  scale_color_manual(values=cpalette[5:6]) +
  scale_fill_manual(values=cpalette[5:6]) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.75, 0.85),
    legend.title = element_blank()
  )

tikz(file = "serial_analysis.tex", width = 8, height = 4, standAlone = T)
grid.arrange(g1, g2, nrow=1)
dev.off()
tools::texi2dvi('serial_analysis.tex', pdf = T, clean = T)

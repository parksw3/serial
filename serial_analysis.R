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
  group_by(cohort) %>%
  summarize(
    mean=mean(serial),
    lwr=quantile(serial, 0.025),
    upr=quantile(serial, 0.975)
  )

rr_summ2 <- rr %>%
  mutate(cohort=`Seconday - symptom onset date`) %>%
  group_by(cohort)  %>%
  summarize(
    mean=mean(serial),
    lwr=quantile(serial, 0.025),
    upr=quantile(serial, 0.975)
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

mm <- max(c(rr$`Seconday - symptom onset date`, rr$`Index - symptom onset date`))

cdata <- data.frame(
  x=9:30,
  y=30-9:30
)

g1 <- ggplot(rr) +
  geom_line(data=cdata, aes(x, y), lty=2) +
  geom_point(data=rr_summ, aes(cohort, mean)) +
  geom_errorbar(data=rr_summ, aes(cohort, ymin=lwr, ymax=upr), width=0) +
  geom_smooth(aes(cohort, serial), col=1, fill=1, fullrange=TRUE, se=FALSE) +
  scale_x_continuous("Symptom onset day (infector)", expand=c(0, 0), limits=c(-3, 29)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(-12, 21)) +
  scale_size_area(max_size=4) +
  coord_cartesian(clip="off", default=TRUE) +
  ggtitle("A. Forward serial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g2 <- ggplot(rr) +
  geom_point(data=rr_summ2, aes(cohort, mean)) +
  geom_errorbar(data=rr_summ2, aes(cohort, ymin=lwr, ymax=upr), width=0) +
  geom_smooth(aes(`Seconday - symptom onset date`, serial), col=1, fill=1, se=FALSE) +
  scale_x_continuous("Symptom onset day (infectee)", expand=c(0, 0)) +
  scale_y_continuous("Backward delay (days)") +
  coord_cartesian(clip="off", default=TRUE) +
  ggtitle("B. Backward serial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

g3 <- ggplot(serall) +
  geom_ribbon(aes(t, ymin=lwr, ymax=upr, fill=type, lty=type, col=type), alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_line(aes(t, R0, col=type, lty=type)) +
  scale_x_continuous("Time (days)", limits=c(-3, 29), expand=c(0, 0)) +
  scale_y_continuous("Basic reproduction number $\\mathcal R_0$") +
  scale_color_manual(values=cpalette[5:6]) +
  scale_fill_manual(values=cpalette[5:6]) +
  coord_cartesian(clip="off", default=TRUE) +
  ggtitle("C. Cohort-averaged estimates") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.68, 0.85),
    legend.title = element_blank()
  )

tikz(file = "serial_analysis.tex", width = 9, height = 3, standAlone = T)
grid.arrange(g1, g2, g3, nrow=1)
dev.off()
tools::texi2dvi('serial_analysis.tex', pdf = T, clean = T)

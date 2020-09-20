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

## day 12 corresponds to 2020-01-22
## figured out based on https://github.com/MeyersLabUTexas/COVID-19/blob/master/Table%20S5%20medrxiv.xlsx

rr <- read_xlsx("Table S5.xlsx", skip=1) %>%
  mutate(
    cohort=`Index - symptom onset date`,
    cohort2=`Seconday - symptom onset date`,
    serial=`Seconday - symptom onset date`- `Index - symptom onset date`
  )

date_range <- range(c(rr$`Index - symptom onset date`, rr$`Seconday - symptom onset date`))

date_numeric <- seq(date_range[1], date_range[2], by=1)
date_actual <- as.Date("2020-01-22") + date_numeric - 12

rr_summ <- rr %>%
  group_by(cohort) %>%
  summarize(
    mean=mean(serial),
    lwr=quantile(serial, 0.025),
    upr=quantile(serial, 0.975)
  ) %>%
  mutate(
    cohort=date_actual[match(cohort, date_numeric)]
  )

rr_summ2 <- rr %>%
  mutate(cohort=`Seconday - symptom onset date`) %>%
  group_by(cohort)  %>%
  summarize(
    mean=mean(serial),
    lwr=quantile(serial, 0.025),
    upr=quantile(serial, 0.975)
  ) %>%
  mutate(
    cohort=date_actual[match(cohort, date_numeric)]
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

ww <- duplicated(c(rr$`Index ID`, rr$`Secondary ID`))

dd <- c(rr$`Index - symptom onset date`, rr$`Seconday - symptom onset date`)[!ww]

sdate <- data.frame(
  date=date_actual[match(dd, date_numeric)]
)

g1 <- ggplot(sdate) +
  geom_bar(aes(date)) +
  scale_x_date("Symptom onset date", expand=c(0, 0)) +
  scale_y_continuous("Number of cases with a known infector/infectee pair", expand=c(0, 0), limits=c(0, 69)) +
  ggtitle("A. Epidemic curve") +
  theme(
    panel.grid = element_blank()
  )

mm <- max(c(rr$`Seconday - symptom onset date`, rr$`Index - symptom onset date`))
mm2 <- min(c(rr$`Seconday - symptom onset date`, rr$`Index - symptom onset date`))

cdata <- data.frame(
  x=date_actual[match(9:29, date_numeric)],
  y=mm-9:29
)

cdata2 <- data.frame(
  x=date_actual[match(-3:9, date_numeric)],
  y=mm2-((-3):9)
)

rr$cohort <- date_actual[match(rr$cohort, date_numeric)]

g2 <- ggplot(rr) +
  annotate("text", x=as.Date("2020-01-30")+0.5, y=11.5, label="maximum observable", angle=-45) +
  annotate("text", x=as.Date("2020-01-14")+0.5, y=-5.5, label="minimum observable", angle=-45) +
  geom_line(data=cdata, aes(x, y), lty=2) +
  geom_line(data=cdata2, aes(x, y), lty=2) +
  geom_point(data=rr_summ, aes(cohort, mean)) +
  geom_errorbar(data=rr_summ, aes(cohort, ymin=lwr, ymax=upr), width=0) +
  geom_smooth(aes(cohort, serial), col=1, fill=1, fullrange=TRUE, se=FALSE) +
  scale_x_date("Symptom onset date (infector)", expand=c(0, 0)) +
  scale_y_continuous("Forward delay (days)", expand=c(0, 0), limits=c(-12, 21)) +
  scale_size_area(max_size=4) +
  coord_fixed(clip="off") +
  ggtitle("B. Forward serial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

mm3 <- max(c(rr$`Seconday - symptom onset date`, rr$`Index - symptom onset date`))
mm4 <- min(c(rr$`Seconday - symptom onset date`, rr$`Index - symptom onset date`))

cdata3 <- data.frame(
  x=date_actual[match(0:16, date_numeric)],
  y=0:16-mm2
)

cdata4 <- data.frame(
  x=date_actual[match(19:30, date_numeric)],
  y=19:30-mm
)

rr$cohort2 <- date_actual[match(rr$cohort2, date_numeric)]

g3 <- ggplot(rr) +
  annotate("text", x=as.Date("2020-01-16")+0.5, y=11.5, label="maximum observable", angle=45) +
  annotate("text", x=as.Date("2020-02-02")+0.5, y=-4.5, label="minimum observable", angle=45) +
  geom_line(data=cdata3, aes(x, y), lty=2) +
  geom_line(data=cdata4, aes(x, y), lty=2) +
  geom_point(data=rr_summ2, aes(cohort, mean)) +
  geom_errorbar(data=rr_summ2, aes(cohort, ymin=lwr, ymax=upr), width=0) +
  geom_smooth(aes(cohort2, serial), col=1, fill=1, se=FALSE) +
  scale_x_date("Symptom onset date (infectee)", expand=c(0, 0)) +
  scale_y_continuous("Backward delay (days)", expand=c(0, 0), limits=c(-11, 19)) +
  coord_fixed(clip="off") +
  ggtitle("C. Backward serial interval") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

serall$date <- serall$t + as.Date("2020-01-22") - 12

g4 <- ggplot(serall) +
  geom_ribbon(aes(date, ymin=lwr, ymax=upr, fill=type, lty=type, col=type), alpha=0.5) +
  geom_hline(yintercept=1, lty=2) +
  geom_line(aes(date, R0, col=type, lty=type)) +
  scale_x_date("Symptom onset date (infector)", limits=as.Date(c("2020-01-07", "2020-02-08")), expand=c(0, 0)) +
  scale_y_continuous("Basic reproduction number $\\mathcal R_0$") +
  scale_color_manual(values=cpalette[5:6]) +
  scale_fill_manual(values=cpalette[5:6]) +
  coord_cartesian(clip="off", default=TRUE) +
  ggtitle("D. Cohort-averaged $\\mathcal R_0$ estimates") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.68, 0.85),
    legend.title = element_blank()
  )

tikz(file = "serial_analysis.tex", width = 8, height = 8, standAlone = T)
grid.arrange(g1, g2, g3, g4, nrow=2)
dev.off()
tools::texi2dvi('serial_analysis.tex', pdf = T, clean = T)

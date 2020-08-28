library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(tikzDevice)
library(gridExtra)
source("cpalette.R")
source("rRfun.R")

## incubation!
meanlog1 <- 1.62
sdlog1 <- 0.42
mean(rlnorm(50000, meanlog = 1.62, sdlog=0.42)) ## 5.5 days
sd(rlnorm(50000, meanlog = 1.62, sdlog=0.42)) ## 2.4

## generation
## moment matching
mean(rlnorm(50000, meanlog = 1.54, sdlog=0.37)) ## 5 days
sd(rlnorm(50000, meanlog = 1.54, sdlog=0.37)) ## 1.9 days

ww <- c(1, 6, 11, 16, 21, 26)

rRdata <- list(
  data.frame(
    r=seq(0, 0.3, by=0.01)[ww+4],
    R=sapply(seq(0, 0.3, by=0.01)[ww+4], function(x) serialR(x, rho=0.75)$R),
    type="Forward serial interval ($\\rho=0.75$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01)[ww+3],
    R=sapply(seq(0, 0.3, by=0.01)[ww+3], function(x) serialR(x, rho=0.5)$R),
    type="Forward serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01)[ww+2],
    R=sapply(seq(0, 0.3, by=0.01)[ww+2], function(x) serialR(x, rho=0.25)$R),
    type="Forward serial interval ($\\rho=0.25$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01)[ww+1],
    R=sapply(seq(0, 0.3, by=0.01)[ww+1], function(x) serialR(x, rho=0)$R),
    type="Forward serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01)[ww],
    R=sapply(seq(0, 0.3, by=0.01)[ww], function(x) generationR(x)$R),
    type="Intrinsic generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Intrinsic generation interval", "Forward serial interval ($\\rho=0.75$)",
                              "Forward serial interval ($\\rho=0.5$)",
                              "Forward serial interval ($\\rho=0.25$)", 
                              "Forward serial interval ($\\rho=0$)"))
  )

g1 <- ggplot(rRdata) +
  geom_smooth(aes(r, R, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, R, col=type, shape=type), size=3) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.3), expand=c(0, 0)) +
  scale_y_continuous("Basic reproduction number $\\mathcal R_0$", limits=c(1, 4.5), expand=c(0, 0)) +
  scale_color_manual(values=cpalette) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.33, 0.75),
    legend.title = element_blank()
  )

rRdata2 <- list(
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR2(x, rho=0.75)$R),
    type="Intrinsic serial interval ($\\rho=0.75$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR2(x, rho=0.5)$R),
    type="Intrinsic serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR2(x, rho=0.25)$R),
    type="Intrinsic serial interval ($\\rho=0.25$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR2(x, rho=0)$R),
    type="Intrinsic serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) generationR(x)$R),
    type="Intrinsic generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Intrinsic generation interval", "Intrinsic serial interval ($\\rho=0.75$)",
                              "Intrinsic serial interval ($\\rho=0.5$)",
                              "Intrinsic serial interval ($\\rho=0.25$)",
                              "Intrinsic serial interval ($\\rho=0$)"))
    )

g2 <- ggplot(rRdata2) +
  geom_smooth(aes(r, R, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, R, col=type, shape=type), size=3) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.3), expand=c(0, 0)) +
  scale_y_continuous("Reproduction number ${\\mathcal R}_{\\textrm{\\tiny intrinsic}}$", limits=c(1, 4.5), expand=c(0, 0)) +
  scale_color_viridis_d(option="C") +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.33, 0.75),
    legend.title = element_blank()
  )

rGdata <- list(
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0.75)$mean),
    type="Serial interval ($\\rho=0.75$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0.5)$mean),
    type="Serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0.25)$mean),
    type="Serial interval ($\\rho=0.25$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0)$mean),
    type="Serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) generationR(x)$mean),
    type="Generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Generation interval", "Serial interval ($\\rho=0.75$)",
                              "Serial interval ($\\rho=0.5$)", 
                              "Serial interval ($\\rho=0.25$)", 
                              "Serial interval ($\\rho=0$)"))
  )

g3 <- ggplot(rGdata) +
  geom_smooth(aes(r, mean, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, mean, col=type, shape=type), size=3) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.3), expand=c(0, 0)) +
  scale_y_continuous("Mean interval (days)", expand=c(0, 0), limits=c(4.7, NA),
                     breaks=c(4:7)) +
  scale_color_manual(values=cpalette) +
  ggtitle("C") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

rkappadata <- list(
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0.75)$kappa),
    type="Serial interval ($\\rho=0.75$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0.5)$kappa),
    type="Serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0.25)$kappa),
    type="Serial interval ($\\rho=0.25$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0)$kappa),
    type="Serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) generationR(x)$kappa),
    type="Generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Generation interval", "Serial interval ($\\rho=0.75$)",
                              "Serial interval ($\\rho=0.5$)", 
                              "Serial interval ($\\rho=0.25$)", 
                              "Serial interval ($\\rho=0$)"))
  )

g4 <- ggplot(rkappadata) +
  geom_smooth(aes(r, mean, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, mean, col=type, shape=type), size=3) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.3), expand=c(0, 0)) +
  scale_y_continuous("Squared coefficient of variation", expand=c(0, 0), limits=c(0, 0.84)) +
  scale_color_manual(values=cpalette) +
  ggtitle("D") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

tikz(file = "rR.tex", width = 8, height = 6, standAlone = T)
grid.arrange(g1, g2, g3, g4, nrow=2)
dev.off()
tools::texi2dvi('rR.tex', pdf = T, clean = T)

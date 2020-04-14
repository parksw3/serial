library(dplyr)
library(ggplot2); theme_set(theme_bw())
library(tikzDevice)
library(gridExtra)
source("cpalette.R")

## incubation!
meanlog1 <- 1.621
sdlog1 <- 0.418
## generation
## moment matching
mean(rlnorm(50000, meanlog = 1.54, sdlog=0.37)) ## 5 days
sd(rlnorm(50000, meanlog = 1.54, sdlog=0.37)) ## 1.9 days

serialR <- function(r=0.1,
                    rho=0.5,
                    meanlog1=1.621,
                    sdlog1=0.418,
                    meanlog2=1.54,
                    sdlog2=0.37,
                    nsim=100000,
                    seed=101) {
  set.seed(seed)
  backinc <- rlnorm(nsim, meanlog=meanlog1, sdlog=sdlog1)
  
  backinc <- sample(backinc, replace=TRUE, prob=exp(-r*backinc))
  
  logbackinc <- log(backinc)
  
  muloggen <- meanlog2 + sdlog2 * rho * (logbackinc - meanlog1)/sdlog1
  sigmaloggen <- sqrt(sdlog2^2 * (1 - rho^2))
  
  forwardgen <- exp(rnorm(nsim, muloggen, sigmaloggen))
  forwardinc <- rlnorm(nsim, meanlog=meanlog1, sdlog=sdlog1)
  
  ser <- -backinc + forwardgen + forwardinc
  
  list(
    mean=mean(ser),
    kappa=var(ser)/mean(ser)^2,
    R=1/mean(exp(-r*ser))
  )
}

serialR2 <- function(r=0.1,
                    rho=0.5,
                    meanlog1=1.621,
                    sdlog1=0.418,
                    meanlog2=1.54,
                    sdlog2=0.37,
                    nsim=100000,
                    seed=101) {
  set.seed(seed)
  backinc <- rlnorm(nsim, meanlog=meanlog1, sdlog=sdlog1)
  
  logbackinc <- log(backinc)
  
  muloggen <- meanlog2 + sdlog2 * rho * (logbackinc - meanlog1)/sdlog1
  sigmaloggen <- sqrt(sdlog2^2 * (1 - rho^2))
  
  forwardgen <- exp(rnorm(nsim, muloggen, sigmaloggen))
  forwardinc <- rlnorm(nsim, meanlog=meanlog1, sdlog=sdlog1)
  
  ser <- -backinc + forwardgen + forwardinc
  
  list(
    mean=mean(ser),
    kappa=var(ser)/mean(ser)^2,
    R=1/mean(exp(-r*ser))
  )
}

generationR <- function(r=0.1,
                        meanlog2=1.54,
                        sdlog2=0.37,
                        nsim=100000) {
  gen <- rlnorm(nsim, meanlog2, sdlog2)
                          
  list(
    mean=mean(gen),
    kappa=var(gen)/mean(gen)^2,
    R=1/mean(exp(-r*gen))
  )
}

rRdata <- list(
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0.5)$R),
    type="Serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0)$R),
    type="Serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=-0.5)$R),
    type="Serial interval ($\\rho=-0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) generationR(x)$R),
    type="Generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Generation interval", "Serial interval ($\\rho=0.5$)",
                        "Serial interval ($\\rho=0$)", "Serial interval ($\\rho=-0.5$)"))
  )

g1 <- ggplot(rRdata) +
  geom_smooth(aes(r, R, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, R, col=type, shape=type), size=2) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.3), expand=c(0, 0)) +
  scale_y_continuous("Reproduction number $\\mathcal R$", limits=c(1, 4.5), expand=c(0, 0)) +
  scale_color_manual(values=cpalette) +
  ggtitle("A") +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.3, 0.75),
    legend.title = element_blank()
  )

rRdata2 <- list(
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR2(x, rho=0.5)$R),
    type="Serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR2(x, rho=0)$R),
    type="Serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) serialR2(x, rho=-0.5)$R),
    type="Serial interval ($\\rho=-0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    R=sapply(seq(0, 0.3, by=0.01), function(x) generationR(x)$R),
    type="Generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Generation interval", "Serial interval ($\\rho=0.5$)",
                              "Serial interval ($\\rho=0$)", "Serial interval ($\\rho=-0.5$)"))
  )

g2 <- ggplot(rRdata2) +
  geom_smooth(aes(r, R, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, R, col=type, shape=type), size=2) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.3), expand=c(0, 0)) +
  scale_y_continuous("Reproduction number $\\mathcal R$", limits=c(1, 4.5), expand=c(0, 0)) +
  scale_color_manual(values=cpalette) +
  ggtitle("B") +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_blank()
  )

rGdata <- list(
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0.5)$mean),
    type="Serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0)$mean),
    type="Serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=-0.5)$mean),
    type="Serial interval ($\\rho=-0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) generationR(x)$mean),
    type="Generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Generation interval", "Serial interval ($\\rho=0.5$)",
                              "Serial interval ($\\rho=0$)", "Serial interval ($\\rho=-0.5$)"))
  )

g3 <- ggplot(rGdata) +
  geom_smooth(aes(r, mean, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, mean, col=type, shape=type), size=2) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.3), expand=c(0, 0)) +
  scale_y_continuous("Mean generation/serial interval", expand=c(0, 0), limits=c(4.7, NA),
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
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0.5)$kappa),
    type="Serial interval ($\\rho=0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=0)$kappa),
    type="Serial interval ($\\rho=0$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) serialR(x, rho=-0.5)$kappa),
    type="Serial interval ($\\rho=-0.5$)"
  ),
  data.frame(
    r=seq(0, 0.3, by=0.01),
    mean=sapply(seq(0, 0.3, by=0.01), function(x) generationR(x)$kappa),
    type="Generation interval"
  )
) %>%
  bind_rows %>%
  mutate(
    type=factor(type, level=c("Generation interval", "Serial interval ($\\rho=0.5$)",
                              "Serial interval ($\\rho=0$)", "Serial interval ($\\rho=-0.5$)"))
  )

g4 <- ggplot(rkappadata) +
  geom_smooth(aes(r, mean, col=type, lty=type), se=FALSE) +
  geom_point(aes(r, mean, col=type, shape=type), size=2) +
  scale_x_continuous("Exponential growth rate $r$ (1/day)", limits=c(0, 0.3), expand=c(0, 0)) +
  scale_y_continuous("Squared coefficient of variation", expand=c(0, 0), limits=c(0, 0.64)) +
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

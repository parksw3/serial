library(ggplot2); theme_set(theme_bw())
library(gridExtra)
library(tikzDevice)

arrow <- data.frame(
  x=0,
  y=0,
  x1=4,
  y1=0
)

g1 <- ggplot(arrow) +
  geom_point(aes(0.5, 0), size=3) +
  geom_point(aes(1.5, 0), size=3) +
  geom_point(aes(2.5, 0), size=3) +
  geom_point(aes(3.5, 0), size=3) +
  geom_point(aes(0.5, 0.2), size=3) +
  geom_point(aes(1.5, 0.4), size=3) +
  geom_point(aes(2.5, 0.3), size=3) +
  geom_point(aes(0.5, 0.1), size=3) +
  annotate("text", x=1.5, y=-0.05, label="$p+\\tau_{\\rm i1}$") +
  annotate("text", x=0.5, y=-0.05, label="$p$") +
  annotate("text", x=0.5, y=-0.1, label="Infector (reference cohort)", col="red") +
  annotate("text", x=2.5, y=-0.1, label="Infectee") +
  annotate("text", x=2.5, y=-0.05, label="$p+\\tau_{\\rm g}$") +
  annotate("text", x=3.5, y=-0.05, label="$p+\\tau_{\\rm g}+\\tau_{\\rm i2}$") +
  geom_segment(aes(x, y, xend=x1, yend=y1), arrow = arrow(length = unit(0.2, "inches")), lwd=1) +
  geom_segment(aes(0.5, 0, xend=0.5, yend=0.2), lty=2) +
  geom_segment(aes(1.5, 0, xend=1.5, yend=0.4), lty=2) +
  geom_segment(aes(2.5, 0, xend=2.5, yend=0.3), lty=2) +
  geom_segment(aes(3.5, 0, xend=3.5, yend=0.4), lty=2) +
  geom_segment(aes(0.5, 0.1, xend=1.5, yend=0.1), arrow = arrow(length = unit(0.1, "inches")), lwd=1) +
  geom_segment(aes(0.5, 0.2, xend=2.5, yend=0.2), arrow = arrow(length = unit(0.1, "inches")), lwd=1) +
  geom_segment(aes(2.5, 0.3, xend=3.5, yend=0.3), arrow = arrow(length = unit(0.1, "inches")), lwd=1) +
  geom_segment(aes(1.5, 0.4, xend=3.5, yend=0.4), arrow = arrow(length = unit(0.1, "inches")), lwd=1.5) +
  annotate("text", x=2.5, y=0.45, label="Intrinsic serial interval, $\\tau_{\\rm s} = (\\tau_{\\rm g} + \\tau_{\\rm i2}) - \\tau_{\\rm i1}$") +
  annotate("text", x=1, y=0.15, label="Intrinsic incubation period, $\\tau_{\\rm i1}$") +
  annotate("text", x=1, y=0.25, label="Intrinsic generation interval, $\\tau_{\\rm g}$") +
  annotate("text", x=3, y=0.35, label="Intrinsic incubation period, $\\tau_{\\rm i2}$") +
  geom_point(aes(0.5, 0), size=4, shape=21, fill="red", stroke=2) +
  scale_y_continuous(limits=c(-0.15, 0.45)) +
  ggtitle("A. Intrinsic serial interval") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

g2 <- ggplot(arrow) +
  geom_segment(aes(0.5, 0, xend=0.5, yend=0.2), lty=2) +
  geom_segment(aes(1.5, 0, xend=1.5, yend=0.4), lty=2) +
  geom_segment(aes(2.5, 0, xend=2.5, yend=0.3), lty=2) +
  geom_segment(aes(3.5, 0, xend=3.5, yend=0.4), lty=2) +
  geom_point(aes(0.5, 0), size=3) +
  geom_point(aes(1.5, 0), size=3) +
  geom_point(aes(2.5, 0), size=3) +
  geom_point(aes(3.5, 0), size=3) +
  geom_point(aes(0.5, 0.2), size=3, col="#81b214") +
  geom_point(aes(1.5, 0.4), size=3, col="#81b214") +
  geom_point(aes(2.5, 0.3), size=3, col="#81b214") +
  geom_point(aes(1.5, 0.1), size=3, col="#07689f") +
  annotate("text", x=1.5, y=-0.05, label="$p$") +
  annotate("text", x=0.5, y=-0.05, label="$p-\\tau_{\\rm i1}$") +
  annotate("text", x=0.5, y=-0.1, label="Infector") +
  annotate("text", x=1.5, y=-0.1, label="Reference cohort", col="red") +
  annotate("text", x=2.5, y=-0.1, label="Infectee") +
  annotate("text", x=2.5, y=-0.05, label="$p-\\tau_{\\rm i1}+\\tau_{\\rm g}$") +
  annotate("text", x=3.5, y=-0.05, label="$p-\\tau_{\\rm i1}+\\tau_{\\rm g}+\\tau_{\\rm i2}$") +
  geom_segment(aes(x, y, xend=x1, yend=y1), arrow = arrow(length = unit(0.2, "inches")), lwd=1) +
  geom_segment(aes(1.5, 0.1, xend=0.5, yend=0.1), arrow = arrow(length = unit(0.1, "inches")), lwd=1, col="#07689f") +
  geom_segment(aes(0.5, 0.2, xend=2.5, yend=0.2), arrow = arrow(length = unit(0.1, "inches")), lwd=1, col="#81b214") +
  geom_segment(aes(2.5, 0.3, xend=3.5, yend=0.3), arrow = arrow(length = unit(0.1, "inches")), lwd=1, col="#81b214") +
  geom_segment(aes(1.5, 0.4, xend=3.5, yend=0.4), arrow = arrow(length = unit(0.1, "inches")), lwd=1.5, col="#81b214") +
  annotate("text", x=2.5, y=0.45, label="Forward serial interval, $\\tau_{\\rm s} = (\\tau_{\\rm g} + \\tau_{\\rm i2}) - \\tau_{\\rm i1}$") +
  annotate("text", x=1, y=0.15, label="Backward incubation period, $\\tau_{\\rm i1}$") +
  annotate("text", x=1, y=0.25, label="Forward generation interval, $\\tau_{\\rm g}$") +
  annotate("text", x=3, y=0.35, label="Forward incubation period, $\\tau_{\\rm i2}$") +
  geom_point(aes(1.5, 0), size=4, shape=21, fill="red", stroke=2) +
  scale_y_continuous(limits=c(-0.15, 0.45)) +
  ggtitle("B. Forward serial interval") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

g3 <- ggplot(arrow) +
  geom_segment(aes(0.5, 0, xend=0.5, yend=0.3), lty=2) +
  geom_segment(aes(1.5, 0, xend=1.5, yend=0.4), lty=2) +
  geom_segment(aes(2.5, 0, xend=2.5, yend=0.2), lty=2) +
  geom_segment(aes(3.5, 0, xend=3.5, yend=0.4), lty=2) +
  geom_point(aes(0.5, 0), size=3) +
  geom_point(aes(1.5, 0), size=3) +
  geom_point(aes(2.5, 0), size=3) +
  geom_point(aes(3.5, 0), size=3) +
  geom_point(aes(3.5, 0.1), size=3, col="#07689f") +
  geom_point(aes(0.5, 0.3), size=3, col="#81b214") +
  geom_point(aes(3.5, 0.4), size=3, col="#81b214") +
  geom_point(aes(2.5, 0.2), size=3, col="#07689f") +
  annotate("text", x=3.5, y=-0.05, label="$s$") +
  annotate("text", x=2.5, y=-0.05, label="$s-\\tau_{\\rm i2}$") +
  annotate("text", x=0.5, y=-0.05, label="$s-\\tau_{\\rm i2}-\\tau_{\\rm g}$") +
  annotate("text", x=1.5, y=-0.05, label="$s-\\tau_{\\rm i2}-\\tau_{\\rm g}+\\tau_{\\rm i1}$") +
  annotate("text", x=0.5, y=-0.1, label="Infector") +
  annotate("text", x=2.5, y=-0.1, label="Infectee") +
  annotate("text", x=3.5, y=-0.1, label="Reference cohort", col="red") +
  geom_segment(aes(x, y, xend=x1, yend=y1), arrow = arrow(length = unit(0.2, "inches")), lwd=1) +
  geom_segment(aes(3.5, 0.1, xend=2.5, yend=0.1), arrow = arrow(length = unit(0.1, "inches")), lwd=1, col="#07689f") +
  geom_segment(aes(2.5, 0.2, xend=0.5, yend=0.2), arrow = arrow(length = unit(0.1, "inches")), lwd=1, col="#07689f") +
  geom_segment(aes(0.5, 0.3, xend=1.5, yend=0.3), arrow = arrow(length = unit(0.1, "inches")), lwd=1, col="#81b214") +
  geom_segment(aes(3.5, 0.4, xend=1.5, yend=0.4), arrow = arrow(length = unit(0.1, "inches")), lwd=1.5, col="#81b214") +
  annotate("text", x=2.5, y=0.45, label="Backward serial interval, $\\tau_{\\rm s} = (\\tau_{\\rm g} + \\tau_{\\rm i2}) - \\tau_{\\rm i1}$") +
  annotate("text", x=3, y=0.15, label="Backward incubation period, $\\tau_{\\rm i2}$") +
  annotate("text", x=2, y=0.25, label="Backward generation interval, $\\tau_{\\rm g}$") +
  annotate("text", x=1, y=0.35, label="Forward incubation period, $\\tau_{\\rm i1}$") +
  geom_point(aes(3.5, 0), size=4, shape=21, fill="red", stroke=2) +
  scale_y_continuous(limits=c(-0.15, 0.45)) +
  ggtitle("C. Backward serial interval") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

tikz(file = "serial_guide.tex", width = 10, height = 8, standAlone = T)
grid.arrange(g1, g2, g3, nrow=3)
dev.off()
tools::texi2dvi('serial_guide.tex', pdf = T, clean = T)

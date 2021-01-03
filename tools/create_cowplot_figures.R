# Auxiliary plots -------------------------------------------------------------
# This script creates the cowplot-based figures used in the README file and
# vignettes

# load packages required to create plots --------------------------------------
library(corrmorant)
library(cowplot)
library(dplyr)

# corrmorant() example plots --------------------------------------------------
c1 <- corrmorant(drosera, style = "light")
c2 <- corrmorant(drosera, style = "dark")
c3 <- corrmorant(drosera, style = "blue_red")
plot1 <- plot_grid(c1,c2,c3 + theme(legend.position = "none"),
          get_legend(c3), nrow = 1, rel_widths = c(1,1,1,0.3))

ggsave("vignettes/corrmorant_examples.png", plot1,
       width = 12, height = 3.8, units = "in")

# Examples for different corrmorant layers ------------------------------------
## Layers for diagonal panels
set.seed(5)
dat <- tibble(V1 = rnorm(50),
              V2 = rnorm(50, V1),
              V3 = rnorm(50, -(2 * V1 - V2)))

p0 <- ggcorrm(dat, rescale = "by_sd") +
  theme(legend.position = "none",
        axis.text = element_text(size = 5)) +
  scale_color_corr(aesthetics = c("fill", "colour"))

p1 <- p0 + dia_names() +
  ggtitle("dia_names()")

p2 <- p0 + dia_density() +
  ggtitle("dia_density()")

p3 <- p0 + dia_histogram() +
  ggtitle("dia_histogram()")

p4 <- p0 + dia_freqpoly() +
  ggtitle("dia_freqpoly()")

# Layers for triangular panels
p1a <- p0 + lotri_corrtext(aes(col = .corr)) +
  ggtitle("lotri_corrtext()")

p2a <- p0 + lotri_funtext(fun = function(x, y) "fun(x, y)") +
  ggtitle("lotri_funtext()")

p3a <- p0 + lotri_heatmap() +
  ggtitle("lotri_heatmap()")

p4a <- p0 + lotri_heatpoint(mapping = aes(col = .corr), pch = 18) +
  ggtitle("lotri_heatpoint()")

p5a <- p0 + lotri_heatcircle(col = 1, size = .3) +
  ggtitle("lotri_heatcircle()")

p1b <- p0 + utri_corrtext(aes(col = .corr)) +
  ggtitle("utri_corrtext()")

p2b <- p0 + utri_funtext(fun = function(x, y) "fun(x, y)") +
  ggtitle("utri_funtext()")

p3b <- p0 + utri_heatmap() +
  ggtitle("utri_heatmap()")

p4b <- p0 + utri_heatpoint(mapping = aes(col = .corr), pch = 18) +
  ggtitle("utri_heatpoint()")

p5b <- p0 + utri_heatcircle(col = 1, size = .3) +
  ggtitle("utri_heatcircle()")

# titles
titles <- ggplot() +
  annotate("text", x = -1, y = 0, label = "Plot diagonal", fontface = "bold", size = 5) +
  annotate("text", x = 0, y = 0, label = "Lower triangle",  fontface = "bold", size = 5) +
  annotate("text", x = 1, y = 0, label = "Upper triangle",  fontface = "bold", size = 5) +
  xlim(-1.32, 1.4) +
  theme_nothing()


plot2 <- plot_grid(p1, p1a, p1b, p2, p2a, p2b, p3, p3a, p3b,
                   p4, p4a, p4b, NULL, p5a, p5b, ncol = 3)
plot2a <- plot_grid(titles, plot2, ncol = 1, rel_heights = c(0.05, 1))

ggsave("vignettes/corrmorant_funs.png", plot2a,
       height = 11.5, width = 7, units = "in")


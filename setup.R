
library(fpp3)
library(patchwork)

# Set some defaults
options(digits = 3, width = 88)

# Colours to be viridis for continuous scales and Okabe for discrete scales
options(
  ggplot2.continuous.colour="viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)

# Font for graphics to be Fira Sans
ggplot2::theme_set(ggplot2::theme_get() + ggplot2::theme(text = ggplot2::element_text(family = 'Fira Sans')))
# Also in base R plots
quartzFonts(firasans = c("Fira Sans Regular","Fira Sans Bold","Fira Sans Italic","Fira Sans Bold Italic"))
par(family = "firasans")

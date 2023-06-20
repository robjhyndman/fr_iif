
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
knitr::opts_chunk$set(
  dev.args = list(bg = grey(0.9), pointsize = 11)
)

# Font for graphics to be Fira Sans
ggplot2::theme_set(
  ggplot2::theme_get() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Fira Sans"),
      plot.background = element_rect(fill = "#fafafa", color = "#fafafa")
    )
)
# Also in base R plots
quartzFonts(
  sans = c("Fira Sans Regular", "Fira Sans Bold", "Fira Sans Italic", "Fira Sans Bold Italic")
)

glm_summary <- function(
    x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor,
    signif.stars = getOption("show.signif.stars"), ...) {
  x <- summary(x)
  resid <- x$residuals
  df <- x$df
  rdf <- df[2L]
    coefs <- x$coefficients
    if (any(aliased <- x$aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4, dimnames = list(
        cn,
        colnames(coefs)
      ))
      coefs[!aliased, ] <- x$coefficients
    }
    printCoefmat(coefs,
      digits = digits, signif.stars = signif.stars,
      na.print = "NA", ...
    )
}

# Monthly hierarchical tourism data (state/zone/region)
# Read csv file of monthly data
OvernightTrips_Region <- readr::read_csv("tourism/OvernightTrips_2017.csv")[, -(1:3)] |>
  # Replace outlier from Adelaide Hills
  mutate(
    `Adelaide Hills` = case_when(
      `Adelaide Hills` > 80 ~ 10,
      TRUE ~ `Adelaide Hills`
    )
  )
# Convert to tsibble
tourism <- hts::hts(
  ts(OvernightTrips_Region, start = 1998, frequency = 12),
  list(7, c(6, 5, 4, 4, 3, 3, 2), c(2, 2, 1, 4, 4, 1, 3, 1, 3, 6, 7, 3, 4, 3, 2, 3, 3, 4, 2, 3, 1, 1, 1, 2, 2, 3, 4))
) |>
  as_tsibble() |>
  rename(
    state = "Level 1",
    zone = "Level 2",
    region = "Level 3",
    month = index,
    visitors = value
  ) |>
  mutate(
    state = recode(state,
                   A = "NSW",
                   B = "VIC",
                   C = "QLD",
                   D = "SA",
                   E = "WA",
                   F = "TAS",
                   G = "NT"
    ),
    zone = recode(zone,
                  AA = "Metro NSW",
                  AB = "North Coast NSW",
                  AC = "South Coast NSW",
                  AD = "South NSW",
                  AE = "North NSW",
                  AF = "ACT",
                  BA = "Metro VIC",
                  BB = "West Coast VIC",
                  BC = "East Coast VIC",
                  BC = "North East VIC",
                  BD = "North West VIC",
                  CA = "Metro QLD",
                  CB = "Central Coast QLD",
                  CC = "North Coast QLD",
                  CD = "Inland QLD",
                  DA = "Metro SA",
                  DB = "South Coast SA",
                  DC = "Inland SA",
                  DD = "West Coast SA",
                  EA = "West Coast WA",
                  EB = "North WA",
                  EC = "South WA",
                  FA = "South TAS",
                  FB = "North East TAS",
                  FC = "North West TAS",
                  GA = "North Coast NT",
                  GB = "Central NT"
    )
  ) |>
  select(month, everything())

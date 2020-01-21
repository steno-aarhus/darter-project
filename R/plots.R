
library(tidyverse)

ggplot() +
    coord_fixed(xlim = c(1920, 2020), ylim = c(0, 100), expand = FALSE) +
    # Diabetes register starts
    geom_vline(xintercept = 1995, linetype = "longdash",
               colour = "grey30", size = 0.75) +
    # Minimum age of diabetes diagnosis
    geom_hline(yintercept = 30, linetype = "longdash",
               colour = "grey30", size = 0.75) +
    # Early childhood phase age
    geom_hline(yintercept = 7, linetype = "longdash",
               colour = "grey30", size = 0.50) +
    # Youngest possible person at diabetes register creation (if "closed cohort")
    geom_abline(slope = 1, intercept = -1995 + 30,
                size = 0.25, colour = "darkblue") +
    # Current youngest possible person for inclusion in diabetes register (if "open cohort")
    geom_abline(slope = 1, intercept = -2020 + 30,
                size = 0.25, colour = "darkblue") +
    # Hypothetical presently alive oldest person (at 100)
    geom_abline(slope = 1, intercept = -2020 + 100,
                size = 0.25, colour = "darkblue") +
    labs(x = "Year", y = "Age") +
    scale_x_continuous(breaks = seq(from = 1920, to = 2020, by = 10)) +
    scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
    theme_classic()

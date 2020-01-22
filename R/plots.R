
library(tidyverse)

vertical_lines <- function(xintercept) {
    geom_vline(
        xintercept = xintercept,
        linetype = "longdash",
        colour = "grey30",
        size = 0.5
    )
}

horizontal_lines <- function(yintercept) {
    geom_hline(
        yintercept = yintercept,
        linetype = "longdash",
        colour = "grey30",
        size = 0.5
    )
}

diagonal_lines <- function(intercept) {
    geom_abline(
        slope = 1,
        intercept = intercept,
        size = 0.3,
        colour = "darkblue",
        linetype = "longdash"
    )
}

text_description <- function(text, x, y) {
    geom_text(aes(x = x, y = y, label = text), hjust = "left")
}

arrow_line <- function(start, end) {

}

ggplot() +
    coord_fixed(xlim = c(1920, 2020), ylim = c(0, 100), expand = FALSE) +
    # Diabetes register starts
    vertical_lines(1995) +
    # Available parental CPR data
    vertical_lines(1960) +
    # Minimum age of diabetes diagnosis
    horizontal_lines(30) +
    # Early childhood phase age
    horizontal_lines(7) +
    # Youngest possible person at diabetes register creation (if "closed cohort")
    diagonal_lines(-1995 + 30) +
    # Current youngest possible person for inclusion in diabetes register (if "open cohort")
    diagonal_lines(-2020 + 30) +
    # Hypothetical presently alive oldest person (at 100)
    diagonal_lines(-2020 + 100) +
    text_description("Age at start of followup", x = 1930, y = 32) +
    labs(x = "Calender Year", y = "Age") +
    scale_x_continuous(breaks = seq(from = 1920, to = 2020, by = 10)) +
    scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
    theme_classic()

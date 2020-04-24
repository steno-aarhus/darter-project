
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

text_description <- function(text, x, y, alignment = "left") {
    geom_text(aes(x = x, y = y, label = text), hjust = alignment)
}

horizontal_arrow <- function(y, xstart, xend, arrow_end = "last") {
    geom_segment(
        aes(
            x = xstart,
            xend = xend,
            y = y,
            yend = y
        ),
        arrow = grid::arrow(
            angle = 25,
            length = grid::unit(0.20, "cm"),
            type = "closed",
            ends = arrow_end
        )
    )
}

vertical_arrow <- function(x, ystart, yend, arrow_end = "last") {
    geom_segment(
        aes(
            y = ystart,
            yend = yend,
            x = x,
            xend = x
        ),
        arrow = grid::arrow(
            angle = 25,
            length = grid::unit(0.20, "cm"),
            type = "closed",
            ends = arrow_end
        )
    )
}

lexis_diagram <- ggplot() +
    coord_cartesian(xlim = c(1920, 2020), ylim = c(0, 100), expand = FALSE) +
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

    # Age at followup
    text_description("Age at start of followup", x = 1930, y = 32) +

    # Early childhood phase
    text_description("Early childhood", x = 1931, y = 3.5) +
    vertical_arrow(1930, 0, 7, "both") +

    # Study period
    text_description("Study period", x = 2007.5, y = 4.5, alignment = "middle") +
    horizontal_arrow(2.5, 1995, 2020, "both") +

    # Prevalent cases
    text_description("Prevalent cases", x = 1994, y = 90, alignment = "right") +
    horizontal_arrow(88, 1995, 1980) +

    # Incident cases
    text_description("Incident cases", x = 1996, y = 90) +
    horizontal_arrow(88, 1995, 2010) +

    # Plot visuals and themes
    labs(x = "Calender Year", y = "Age") +
    scale_x_continuous(breaks = seq(from = 1920, to = 2020, by = 10)) +
    scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
    theme_classic()

ggsave(
    here::here("images/lexis-diagram.pdf"),
    lexis_diagram,
    width = 10,
    height = 6
)

ggsave(
    here::here("images/lexis-diagram.svg"),
    lexis_diagram,
    width = 10,
    height = 6
)

ggplot() +
    coord_cartesian(xlim = c(1950, 2020), ylim = c(0, 70), expand = FALSE) +
    # Diabetes register starts
    vertical_lines(1995) +

    # Available parental CPR data
    vertical_lines(1968) +

    # Minimum age of diabetes diagnosis
    horizontal_lines(30) +

    # Early childhood phase age
    horizontal_lines(7) +

    # Current youngest possible person for inclusion in diabetes register
    diagonal_lines(-2020 + 30) +

    # Oldest possible person based on CPR data
    diagonal_lines(-1968) +

    # Earliest age of T2D diagnosis
    # text_description("Earlis", x = 1960, y = 32) +

    # Early childhood phase
    text_description("Early childhood", x = 1961, y = 3.5) +
    vertical_arrow(1960, 0, 7, "both") +

    # Study period
    text_description("Study period", x = 2007.5, y = 4.5, alignment = "middle") +
    horizontal_arrow(2.5, 1995, 2020, "both") +

    # Prevalent cases
    text_description("Prevalent cases", x = 1994, y = 60, alignment = "right") +
    horizontal_arrow(58, 1995, 1980) +

    # Incident cases
    text_description("Incident cases", x = 1996, y = 60) +
    horizontal_arrow(58, 1995, 2010) +

    # Plot visuals and themes
    labs(x = "Calender Year", y = "Age") +
    scale_x_continuous(breaks = seq(from = 1950, to = 2020, by = 10)) +
    scale_y_continuous(breaks = seq(from = 0, to = 70, by = 10)) +
    theme_classic()

# Tracking progress on which registers and their variables to use for the application.
# Use to update the Issue tracking this.

library(tidyverse)
library(glue)
library(here)
source(here("R/functions.R"))

# Functions ---------------------------------------------------------------

create_checklist <- function(data, from_dst = TRUE, type = c("scraped", "inspected")) {
    condition <- data$owner == "dst"
    prepended_text <- "## From DST\n"
    if (!from_dst) {
        condition <- data$owner != "dst" | is.na(data$owner)
        prepended_text <- "## (Maybe) Not from DST\n"
    }

    glue_text <- paste0("* [{", type, "}] {register_id}: {register_name_english}")

    data %>%
        dplyr::filter(condition) %>%
        glue::glue_data(glue_text, .na = "?") %>%
        purrr::prepend(prepended_text) %>%
        clipr::write_clip()
}

#  Checks -----------------------------------------------------------------

compare_current_vs_needed_dst_registers()

# Needed registers importing in -------------------------------------------

needed_registers <- read_csv(here::here("data/needed-registers.csv")) %>%
    mutate(inspected = if_else(is.na(inspected), " ", "X"),
           scraped = if_else(is.na(scraped), " ", "X")) %>%
    arrange(register_id)

# current_sds_registers <-
    # here("data/sds-registers-with-variables-to-use.csv") %>%
    # read_csv() %>%
    #     count(register_name)
    #     col_types = cols_only(register_id = col_character())) %>%
    # pull(register_id) %>%
    # unique()

# Registers that have been scraped ----------------------------------------

create_checklist(needed_registers, from_dst = TRUE, type = "scraped")
create_checklist(needed_registers, from_dst = FALSE, type = "scraped")

# Registers that have been reviewed ---------------------------------------

create_checklist(needed_registers, from_dst = TRUE, type = "inspected")
create_checklist(needed_registers, from_dst = FALSE, type = "inspected")

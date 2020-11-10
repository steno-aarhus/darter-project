# Tracking progress on which registers and their variables to use for the application.
# Use to update the Issue tracking this.

library(tidyverse)
library(glue)

needed_registers <- read_csv(here::here("data/needed-registers.csv"))

needed_registers %>%
    mutate(
        completed = if_else(is.na(completed), " ", "X")
    ) %>%
    glue_data("* [{completed}] {register_id}: {register_name_english}. Owner: {owner}",
              .na = "?") %>%
    clipr::write_clip()

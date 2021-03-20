
library(tidyverse)
library(glue)
library(here)

# Comments and unresolved issues
read_csv(here("data/needed-registers.csv"),
         col_types = cols_only(
             drop = col_logical(),
             register_id = col_character(),
             register_name_english = col_character(),
             note = col_character()
         )) %>%
    filter(!drop | is.na(drop), !is.na(note)) %>%
    glue_data("- {register_id} ({register_name_english}): {note}") %>%
    clipr::write_clip()

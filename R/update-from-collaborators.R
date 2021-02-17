
library(tidyverse)
library(glue)
library(here)
library(janitor)

from_collabs <- here("data/data_dst-registers-with-variables-to-use.csv") %>%
    read_csv() %>%
    rename(collab_use_in_application = use_in_application,
           collab_reason_for_use = reason_for_use)

original <- here("data/dst-registers-with-variables-to-use.csv") %>%
    read_csv()

updated <- full_join(original,
                     from_collabs) %>%
    relocate(starts_with("collab"))

# check for duplicates
updated %>%
    get_dupes(-c(contains("use_in_application"), contains("reason_for_use"))) %>%
    view()

# To check if the updated has any that the original doesn't.
# Mostly to check that there are no conversion errors if Excel was used... -_-
updated %>%
    anti_join(original) %>%
    view()

# Can now save it.
write_csv(updated, here("data/dst-registers-with-variables-to-use.csv"),
          na = "")

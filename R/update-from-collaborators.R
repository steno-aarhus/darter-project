library(tidyverse)
library(glue)
library(here)
library(janitor)

from_collabs <- here("data/data_dst-registers-with-variables-to-use.csv") %>%
  read_csv() %>%
  rename(
    collab_use_in_application = use_in_application,
    collab_reason_for_use = reason_for_use
  )

original <- here("data/dst-registers-with-variables-to-use.csv") %>%
  read_csv()

updated <- full_join(
  original,
  from_collabs
) %>%
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
  na = ""
)

# New collaborator added --------------------------------------------------

## For DST
original_dst <- here("data/dst-registers-with-variables-to-use.csv") %>%
  read_csv()

updates_dst <- read_csv2(
  # Deleted this file, not needed any longer.
  here("data-raw/addition-dst.csv"),
  col_names = c(
    "variable_name", "description_english",
    "register_id", "time_period"
  ),
  locale = locale(encoding = "latin1")
) %>%
  mutate(
    register_id = str_remove(register_id, " - .*$"),
    variable_name = str_replace(variable_name, "pnr/cpr", "pnr12"),
    use_in_application = 1
  ) %>%
  separate(time_period,
    into = c("register_start_year", "register_last_year"),
    sep = "-", convert = TRUE
  )

# registers missing.
missing_registers <- anti_join(
  updates_dst %>% distinct(register_id),
  original_dst %>% distinct(register_id)
)

# variables missing
missing_variables <- anti_join(
  updates_dst %>% distinct(register_id, variable_name),
  original_dst %>% distinct(register_id, variable_name)
)

full_join(original_dst, updates_dst) %>%
  write_csv(here("data/dst-registers-with-variables-to-use.csv"),
    na = ""
  )

## SDS
original_sds <- here("data/sds-registers-with-variables-to-use.csv") %>%
  read_csv()

updates_sds <- read_csv2(
  here("data-raw/addition-sds.csv"),
  col_names = c(
    "table_name",
    "variable_name",
    "variablebeskrivelse",
    "periode",
    "use_in_application",
    "register_name"
  ),
  locale = locale(encoding = "latin1")
) %>%
  mutate(
    # register_id = str_remove(register_id, " - .*$"),
    use_in_application = as.integer(use_in_application == "x")
  )

# registers missing.
missing_registers <- anti_join(
  updates_sds %>% distinct(register_name),
  original_sds %>% distinct(register_name)
)

# variables missing
missing_variables <- anti_join(
  updates_sds %>% distinct(register_name, variable_name),
  original_sds %>% distinct(register_name, variable_name)
)

full_join(original_sds, updates_sds) %>%
  write_csv(here("data/sds-registers-with-variables-to-use.csv"),
    na = ""
  )

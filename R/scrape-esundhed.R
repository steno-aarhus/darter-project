
library(rvest)
library(tidyverse)
library(glue)
library(snakecase)
conflicted::conflict_prefer("filter", "dplyr")

# Functions ---------------------------------------------------------------

generic_tidy_scrape <- function(.scraped_object, .node_class) {
    object_list <- .scraped_object %>%
        html_node(.node_class) %>%
        html_nodes("a")

    tibble(
        Numbers = object_list %>%
            html_attr("id") %>%
            str_remove("list-(register|table|variable)-"),
        Names = object_list %>%
            html_text()
    )
}

tidy_scraped_registers <- function(.scraped_object) {
    generic_tidy_scrape(.scraped_object = .scraped_object,
                        .node_class = "#list-tab-registers")
}

tidy_scraped_register_tables <- function(.scraped_object) {
    generic_tidy_scrape(.scraped_object = .scraped_object,
                        .node_class = "#list-tab-tables")
}

tidy_scraped_register_variables <- function(.scraped_object) {
    generic_tidy_scrape(.scraped_object = .scraped_object,
                        .node_class = "#list-tab-variables")
}

tidy_scraped_descriptions <- function(.scraped_object) {
    divs_from_page <- .scraped_object %>%
        html_nodes("div")

    divs_to_keep <- divs_from_page %>%
        html_attrs() %>%
        map(~is_empty(unname(.)))

    # TODO: How to add data about table vs variable period
    divs_from_page %>%
        html_text() %>%
        keep(unlist(divs_to_keep)) %>%
        .[-2] %>%
        str_trim() %>%
        str_remove_all("\\n|\\r") %>%
        # Use double colon to differentiate from colons used in sentences.
        # TODO: Fix this here, this should maybe be put after the code below
        str_replace(":", "::") %>%
        str_replace(
            "(Kort om registeret|Lovgivning og anmeldelse|Tabellens indhold|Variablebeskrivelse)",
            "\\1::"
        )
}

scrape_and_tidy <- function(.url_extensions, .tidy_fn) {
    .url_extensions %>%
        map( ~ read_html(glue("{esundhed_base_url}/{.}"))) %>%
        set_names(.url_extensions) %>%
        # TODO: This converts to wide, rather than by row... needs to be fixed
        map_dfr(.tidy_fn, .id = "url_extension")

}

stop("To prevent accidental sourcing.")

# Scraping URL extensions -------------------------------------------------

esundhed_base_url <- "https://www.esundhed.dk/api/sitecore/documentation/documentation"
register_numbers <- esundhed_base_url %>%
    read_html() %>%
    tidy_scraped_registers()

kept_register_numbers <- register_numbers %>%
    filter(Numbers %in% c(5, 9, 12, 17, 18, 19)) %>%
    mutate(url_extension = glue("?rid={Numbers}")) %>%
    rename(register_id = Numbers, register_name = Names)

register_table_numbers <- kept_register_numbers %>%
    pull(url_extension) %>%
    scrape_and_tidy(tidy_scraped_register_tables) %>%
    rename(table_id = Numbers, table_name = Names) %>%
    mutate(url_extension = glue("{url_extension}&tid={table_id}"))

register_variable_numbers <- register_table_numbers %>%
    pull(url_extension) %>%
    scrape_and_tidy(tidy_scraped_register_variables) %>%
    rename(variable_id = Numbers, variable_name = Names) %>%
    mutate(url_extension = glue("{url_extension}&vid={variable_id}"))

# Scraping descriptions ---------------------------------------------------

tidied_descriptions <- register_variable_numbers %>%
    pull(url_extension) %>%
    scrape_and_tidy(tidy_scraped_descriptions)

# Not saved in Git.
write_csv(tidied_descriptions, here::here("data-raw/sds/registers-descriptions.csv"))

full_variable_list <- register_variable_numbers %>%
    mutate(url_extension = url_extension %>%
               str_remove("&vid=.*$") %>%
               str_remove_all("[?&]") %>%
               str_remove_all("[rt]id") %>%
               str_remove("^=")) %>%
    separate(url_extension,
             into = c("register_id", "table_id"),
             sep = "=") %>%
    full_join(select(kept_register_numbers, -url_extension), by = "register_id") %>%
    full_join(select(register_table_numbers, -url_extension), by = "table_id")

# Not saved in Git.
write_csv(full_variable_list, here::here("data-raw/sds/variables.csv"))

# Prepare descriptions ----------------------------------------------------

tidied_descriptions <- read_csv(here::here("data-raw/sds/registers-descriptions.csv"))
full_variable_list <- read_csv(here::here("data-raw/sds/variables.csv"))

tidied_descriptions_long <- tidied_descriptions %>%
    pivot_longer(cols = everything(),
                 names_to = "id",
                 values_to = "variable_description") %>%
    mutate(variable_description = variable_description %>%
               str_replace_all("  +", " ") %>%
               str_replace_all(":: ?", ": ") %>%
               str_replace(": ", "::") %>%
               str_remove_all("\t")) %>%
    separate(variable_description,
             into = c("description_title", "description_text"),
             sep = "::") %>%
    mutate(across(everything(), str_trim))

tidied_variable_list <- full_variable_list %>%
    mutate(id = glue("?rid={register_id}&tid={table_id}&vid={variable_id}") %>%
               as.character())

tidy_prep <- tidied_descriptions_long %>%
    mutate(description_title = description_title %>%
               str_replace_all("æ", "ae") %>%
               to_snake_case()) %>%
    filter(description_text != "",
           description_title != "sidst_aendret",
           !(description_title == "periode" &
                 str_detect(description_text, "Tabellen indeholder kontakter"))) %>%
    group_by(id, description_title) %>%
    add_count()

# tidy_prep %>%
#     filter(n > 1) %>%
#     ungroup() %>%
#     count(description_title)

sds_descriptions <- tidy_prep %>%
    # There are some duplicates here, this fixes many but not all.
    summarise(description_text = unique(description_text),
              .groups = "drop") %>%
    full_join(tidied_variable_list, by = "id") %>%
    pivot_wider(names_from = description_title,
                values_from = description_text,
                values_fn = list) %>%
    unnest(-periode) %>%
    select(-format, -laengde, -funktion, -kilder, -kodesaet,
           -vaerdier, -ends_with("_id"))

# For translation ---------------------------------------------------------

# For translating this file, it's too big to put into Google Translate to
# translate it all at once. So, you'll need to make a copy, open it up in a
# spreadsheet program, delete the first two columns, save as a txt file and then
# upload the file to this location https://translate.google.dk/#view=home&op=docs&sl=auto&tl=en
# After that, open the original csv file and highlight the text until the point
# when Translate stopped. Then, paste the output into a new column for the
# translation, open the copy and delete the rows you just translated, and then
# repeat the process. It shouldn't take too long.
sds_descriptions %>%
    select(-ends_with("_name"), -periode) %>%
    pivot_longer(-id) %>%
    count(name, value) %>%
    rowid_to_column() %>%
    # Not saved in Git.
    write_csv(here::here("data-raw/sds/descriptions-for-translations.csv"))

# Saving final descriptions -----------------------------------------------

# After pasting the translation, we'll join with the translated files with
# the non-translated files, join both dataframes into a single dataframe,
# and save to data/.
sds_descriptions_translated <- sds_descriptions %>%
    select(-ends_with("_name"), -periode) %>%
    pivot_longer(-id) %>%
    count(name, value) %>%
    select(-n) %>%
    rowid_to_column() %>%
    full_join(read_csv(here::here("data-raw/sds/descriptions-for-translations.csv")) %>%
                  select(-name, -value),
              by = "rowid") %>%
    arrange(rowid)

sds_descriptions %>%
    pivot_longer(-c(id, ends_with("_name"), periode)) %>%
    full_join(sds_descriptions_translated %>%
                  select(-rowid),
              by = c("name", "value")) %>%
    mutate(name_english = snakecase::to_snake_case(name_english) %>%
               str_replace("kort_om_registeret", "short_register_description"),
           name_english = case_when(
               str_detect(name_english, "known_dat") ~ "known_data_break_changes",
               str_detect(name_english, "table_content") ~ "table_contents",
               TRUE ~ name_english
           )) %>%
    select(-name, -value) %>%
    pivot_wider(names_from = name_english,
                values_from = value_english) %>%
    full_join(sds_descriptions,
              by = c("id", "variable_name", "register_name", "table_name", "periode")) %>%
    select(-id) %>%
    relocate(register_name, table_name, variable_name) %>%
    unnest(periode) %>%
    mutate(use_in_application = NA_character_,
           reason_for_use = NA_character_,
           .before = 1) %>%
    mutate(across(c(known_data_break_changes, kendte_databrud_aendringer),
                  ~ .x %>%
                      na_if("None") %>%
                      na_if("Ingen"))) %>%
    relocate(short_register_description, table_contents, variable_description,
             .after = variable_name) %>%
    # Not saved in Git.
    write_csv(here::here("data/sds-registers-with-variables.csv"),
              na = "")



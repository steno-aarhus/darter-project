
library(rvest)
# TODO: Since esundhed uses an API, don't need to use polite.
library(polite)
library(tibble)
library(purrr)
library(stringr)
library(glue)
library(tidyr)
library(dplyr)
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
        map( ~ nod(esundhed, glue("{esundhed_api_path}{.}"))) %>%
        map(scrape) %>%
        set_names(.url_extensions) %>%
        # TODO: This converts to wide, rather than by row... needs to be fixed
        map_dfr(.tidy_fn, .id = "url_extension")

}

stop("To prevent accidental sourcing.")

# Scraping ----------------------------------------------------------------

esundhed_api_path <- "api/sitecore/documentation/documentation"
esundhed <- bow("https://www.esundhed.dk")
scraped_register_numbers <- esundhed %>%
    nod(esundhed_api_path) %>%
    scrape() %>%
    tidy_scraped_registers()

kept_registers <- scraped_register_numbers %>%
    filter(Numbers %in% c(5, 9, 12, 17, 18, 19)) %>%
    mutate(url_extension = glue("?rid={Numbers}")) %>%
    rename(register_id = Numbers, register_name = Names)

scraped_table_numbers <- kept_registers %>%
    pull(url_extension) %>%
    scrape_and_tidy(tidy_scraped_register_tables) %>%
    rename(table_id = Numbers, table_name = Names) %>%
    mutate(url_extension = glue("{url_extension}&tid={table_id}"))

scraped_variable_numbers <- scraped_table_numbers %>%
    pull(url_extension) %>%
    scrape_and_tidy(tidy_scraped_register_variables) %>%
    rename(variable_id = Numbers, variable_name = Names) %>%
    mutate(url_extension = glue("{url_extension}&vid={variable_id}"))

tidied_scraped_descriptions <- scraped_variable_numbers %>%
    pull(url_extension) %>%
    # TODO: Fix so it isn't converted into wide format.
    scrape_and_tidy(tidy_scraped_descriptions)

write_csv(tidied_scraped_descriptions, here::here("data-raw/sds/variable-descriptions.csv"))
tidied_scraped_descriptions <- read_csv(here::here("data-raw/sds/variable-descriptions.csv"))

tidied_descriptions <- tidied_scraped_descriptions %>%
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

full_variable_list <- scraped_variable_numbers %>%
    mutate(url_extension = url_extension %>%
               str_remove("&vid=.*$") %>%
               str_remove_all("[?&]") %>%
               str_remove_all("[rt]id") %>%
               str_remove("^=")) %>%
    separate(url_extension,
             into = c("register_id", "table_id"),
             sep = "=") %>%
    full_join(select(kept_registers, -url_extension), by = "register_id") %>%
    full_join(select(scraped_table_numbers, -url_extension), by = "table_id")

write_csv(full_variable_list, here::here("data-raw/sds/variables.csv"))
full_variable_list <- read_csv(here::here("data-raw/sds/variables.csv"))

tidied_variable_list <- full_variable_list %>%
    mutate(id = glue("?rid={register_id}&tid={table_id}&vid={variable_id}") %>%
               as.character())

tidy_prep <- tidied_descriptions %>%
    mutate(description_title = description_title %>%
               str_replace_all("æ", "ae") %>%
               to_snake_case()) %>%
    filter(description_text != "",
           description_title != "sidst_aendret",
           !(description_title == "periode" &
                 str_detect(description_text, "Tabellen indeholder kontakter"))) %>%
    group_by(id, description_title) %>%
    add_count()

tidy_prep %>%
    # There are some duplicates here, this fixes many but not all.
    filter(n > 1) %>%
    summarise(description_text = unique(description_text)) %>%
    ungroup() %>%
    full_join(tidied_variable_list, by = "id") %>%
    pivot_wider(names_from = description_title,
                values_from = description_text,
                values_fn = list) %>%
    View()


# Translate via https://translate.google.dk/#view=home&op=docs&sl=da&tl=en


library(rvest)
library(polite)
library(tibble)
library(purrr)
library(stringr)
library(glue)
library(dplyr)

# TODO: Use maybe Yandex for translation?

# Functions ---------------------------------------------------------------

generic_scraping <- function(.bow_object, .node_class) {
    object_list <- .bow_object %>%
        scrape() %>%
        html_node(.node_class) %>%
        html_nodes("a")

    tibble(
        Numbers = object_list %>%
            html_attr("id") %>%
            str_remove("list-(register|table|variable)-"),
        Names = object_list %>%
            html_text()
        # TODO: Add table description here? Or later when I have all rid, tid, and vid?
    )
}

scrape_registers <- function(.bow_object) {
    generic_scraping(.bow_object = .bow_object,
                     .node_class = "#list-tab-registers")
}

scrape_register_tables <- function(.bow_object) {
    generic_scraping(.bow_object = .bow_object,
                     .node_class = "#list-tab-tables")
}

scrape_register_variables <- function(.bow_object) {
    generic_scraping(.bow_object = .bow_object,
                     .node_class = "#list-tab-variables",
                     .id = "vid")
}
stop("To prevent accidental sourcing.")

# Scraping ----------------------------------------------------------------

esundhed <- bow("https://www.esundhed.dk/api/sitecore/documentation/documentation")
scraped_register_info <- esundhed %>%
    scrape_registers()

kept_registers <- scraped_register_info %>%
    filter(Numbers %in% c(5, 9, 12, 17, 18, 19))

scraped_tables_info <- kept_registers %>%
    pmap_dfr(function(ID, Numbers, Names) {
        new_url <- glue("{esundhed$url}?rid={Numbers}")
        register_id <- Numbers
        register_name <- Names
        table_page <- bow(new_url)
        table_page %>%
            scrape_register_tables() %>%
            mutate(register_id = register_id,
                   register_name = register_name)
    }) %>%
    rename(table_id = Numbers, table_name = Names)

scraped_tables_info %>%
    pmap(function(ID, Numbers, Names) {
        url_extension <- glue("?{ID}={Numbers}")
        register_name <- Names
        table_page <- nod(esundhed, url_extension)
        table_page %>%
            scrape_register_tables() %>%
            mutate(register_id = url_extension,
                   register_name = register_name)
    })


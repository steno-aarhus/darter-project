
library(rvest)
library(polite)
library(tibble)
library(purrr)
library(stringr)
library(glue)
library(tidyr)
library(dplyr)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("pluck", "purrr")
needed_registers <- read_csv(here::here("data/register-list.csv"),
                             col_types = cols("c"))

# Functions ---------------------------------------------------------------

stop("To prevent accidental sourcing.")

nod_and_scrape <- function(host, extension) {
    host %>%
        nod(extension) %>%
        scrape(content = "text/html; charset=windows-1252")
}

# Scraping ----------------------------------------------------------------

# dst_documentation_url_extension <- "da/TilSalg/Forskningsservice/Dokumentation/hoejkvalitetsvariable"
dst_documentation_url_extension <- "extranet/forskningvariabellister/Oversigt%20over%20registre.html"
dst_host <- bow("https://www.dst.dk")

scraped_dst <- dst_host %>%
    nod_and_scrape(dst_documentation_url_extension)

extract_table_cell_links <- function(scraped_html) {
    a_nodes <- scraped_html %>%
        html_node(".table") %>%
        html_nodes("a")

    tibble(
        register = html_text(a_nodes),
        register_url = html_attr(a_nodes, "href")
    )
}

register_names_with_urls <- scraped_dst %>%
    extract_table_cell_links()

register_names_tbl <- scraped_dst %>%
    html_node(".table") %>%
    html_table() %>%
    rename_with(snakecase::to_snake_case) %>%
    full_join(register_names_with_urls, by = "register") %>%
    rename(register_abbreviation = register) %>%
    as_tibble()

needed_registers_tbl <- register_names_tbl %>%
    inner_join(needed_registers) %>%
    mutate(url_extension = str_remove(register_url, "https://www.dst.dk/"))

extract_register_html_table <- function(html) {
    html %>%
        html_node(".table") %>%
        html_table(fill = TRUE, header = FALSE)
}

scraped_register_tables <- needed_registers_tbl$url_extension[1:2] %>%
    map(~nod_and_scrape(dst_host, .x))

register_variable_links <- scraped_register_tables[[1]] %>%
    extract_table_cell_links()

full_join(
    register_variable_links %>%
        filter(str_detect(register_url, "dokumentation/Times/")) %>%
        rename(TIMES = register, variable_times_url = register_url),
    register_variable_links %>%
        filter(!str_detect(register_url, "dokumentation/Times/")) %>%
        mutate(TIMES = register) %>%
        rename(højkvalitetsdokumentation = register, variable_hkd_url = register_url),
    by = "TIMES"
)

%>%
    map(extract_register_html_table)

tidy_dst_register_tables <- function(tbl) {
    tbl %>%
        as_tibble() %>%
        slice(-1:-3) %>%
        mutate(across(-c(X1, X2, X3), as.numeric)) %>%
        rowwise() %>%
        mutate(
            start_year = min(c_across(-c(X1, X2, X3)), na.rm = TRUE),
            end_year = max(c_across(-c(X1, X2, X3)), na.rm = TRUE)
        ) %>%
        select(
            times = X1,
            højkvalitetsdokumentation = X2,
            description = X3,
            start_year,
            end_year
        )
}

cleaned_register_tables <- register_tables %>%
    map(tidy_dst_register_tables)

scraped_test %>%
    html_node(".table") %>%
    html_table(fill = TRUE)


leftover_registers <- needed_registers %>%
    anti_join(register_names_table) %>%
    pull(register_abbreviation)

register_names_table %>%
    filter(str_detect(register_abbreviation, "IND")) %>%
    View()

register_names_table %>%
    filter(første_år < 1980) %>%
    View()

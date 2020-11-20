
library(tidyverse)
library(rvest)
library(polite)
library(glue)
library(here)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("pluck", "purrr")

current_registers <-
    here("data/dst-registers-with-variables.csv") %>%
    read_csv(col_types = cols_only(register_id = col_character())) %>%
    pull(register_id) %>%
    unique()

needed_registers <-
    here("data/needed-registers.csv") %>%
    read_csv(col_types = cols_only(register_id = col_character())) %>%
    filter(!register_id %in% current_registers) %>%
    na.omit()

# Functions ---------------------------------------------------------------

stop("To prevent accidental sourcing.")

repair_url_encoding <- function(url) {
    url %>%
        iconv(from = "UTF-8", to = "ASCII", sub = "byte") %>%
        str_replace_all("<([a-z][0-9])>", "%\\1")
}

nod_and_scrape <- function(host, extension) {
    extension <- repair_url_encoding(extension)

    host %>%
        nod(extension) %>%
        scrape(content = "text/html; charset=windows-1252")
}

extract_table_cell_links <- function(scraped_html) {
    a_nodes <- scraped_html %>%
        html_node(".table") %>%
        html_nodes("a")

    tibble(
        register = html_text(a_nodes),
        register_url = html_attr(a_nodes, "href")
    )
}

extract_variable_table <- function(scraped_html) {
    scraped_html %>%
        html_node(".table") %>%
        html_table(fill = TRUE, header = FALSE)
}

tidy_variable_links <- function(tbl) {
    full_join(
        tbl %>%
            filter(str_detect(register_url, "dokumentation/Times/")) %>%
            rename(times = register, variable_times_url = register_url),
        tbl %>%
            filter(!str_detect(register_url, "dokumentation/Times/")) %>%
            mutate(times = register) %>%
            rename(højkvalitetsdokumentation = register, variable_hkd_url = register_url),
        by = "times"
    )
}

tidy_variable_table <- function(tbl) {
    tbl %>%
        as_tibble() %>%
        # First three rows are table headers from the html source
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

extract_register_variable_table_with_urls <- function(scraped_html) {
    url_links <- scraped_html %>%
        extract_table_cell_links() %>%
        tidy_variable_links()

    variable_table <- scraped_html %>%
        extract_variable_table() %>%
        tidy_variable_table()

    full_join(variable_table, url_links,
              by = c("times", "højkvalitetsdokumentation"))
}

# Scraping ----------------------------------------------------------------

dst_documentation_url_extension <- "extranet/forskningvariabellister/Oversigt%20over%20registre.html"
dst_host <- bow("https://www.dst.dk")

scraped_dst <- dst_host %>%
    nod_and_scrape(dst_documentation_url_extension)

register_names_with_urls <- scraped_dst %>%
    extract_table_cell_links()

register_names_tbl <- scraped_dst %>%
    html_node(".table") %>%
    html_table() %>%
    rename_with(snakecase::to_snake_case) %>%
    full_join(register_names_with_urls, by = "register") %>%
    rename(register_id = register) %>%
    as_tibble()

needed_registers_tbl <- register_names_tbl %>%
    inner_join(needed_registers, by = "register_id") %>%
    mutate(url_extension = str_remove(register_url, "https://www.dst.dk/"))

register_variables_tables <- map2(
    needed_registers_tbl$url_extension,
    needed_registers_tbl$register_id,
    ~ nod_and_scrape(dst_host, .x) %>%
        {print(.y);.} %>%
        extract_register_variable_table_with_urls() %>%
        mutate(register = .y)
)

register_variables_df <- register_variables_tables %>%
    bind_rows() %>%
    select(-højkvalitetsdokumentation) %>%
    relocate(register) %>%
    rename(register_id = register,
           variable_name = times,
           description_dk = description)

write_csv(register_variables_df,
          here::here("data-raw/dst/registers-with-variables.csv"))

registers_df <- needed_registers_tbl %>%
    select(register_id = register_abbreviation, register_name_dk = registertitel,
           start_year = første_år,
           start_month = start_måned, last_year = seneste_år,
           last_month = slut_måned, date_last_update = seneste_opdateringsdato,
           register_url)

write_csv(registers_df,
          here::here("data-raw/dst/registers.csv"))

# For translation ---------------------------------------------------------

# For this file, it is small enough that you can copy and paste into Google
# Translate and than copy and paste back into the spreadsheet.
registers_df %>%
    select(register_id, register_name_dk) %>%
    write_csv(here::here("data-raw/dst/registers-for-translation.csv"))

# For translating this file, it's too big to put into Google Translate to
# translate it all at once. So, you'll need to make a copy, open it up in a
# spreadsheet program, delete the first two columns, save as a txt file and then
# upload the file to this location https://translate.google.dk/#view=home&op=docs&sl=auto&tl=en
# After that, open the original csv file and highlight the text until the point
# when Translate stopped (somewhere around 1200 lines of text). Then,
# paste the output into a new column called `description_english`, open the copy
# and delete the rows you just translated, and then repeat the process. It shouldn't
# take too long.
register_variables_df %>%
    select(register_id, variable_name, description_dk) %>%
    write_csv(here::here("data-raw/dst/registers-with-variables-for-translation.csv"))

# Saving final register list ----------------------------------------------

# This section can be run without doing any of the above.

# After pasting the translation, we'll join with the translated files with
# the non-translated files, join both dataframes into a single dataframe,
# and save to data/.
registers_df <- read_csv(here::here("data-raw/dst/registers.csv"))
registers_translated <- read_csv(here::here("data-raw/dst/registers-for-translation.csv")) %>%
    full_join(registers_df, by = c("register_id", "register_name_dk"))

register_variables_df <- read_csv(here::here("data-raw/dst/registers-with-variables.csv"))
variables_translated <- read_csv(here::here("data-raw/dst/registers-with-variables-for-translation.csv")) %>%
    full_join(register_variables_df, by = c("register_id", "variable_name", "description_dk"))

registers_translated %>%
    rename_with( ~ str_c("register_", .x), contains("year")) %>%
    full_join(variables_translated,
              by = "register_id") %>%
    rename(register_start_month = start_month,
           register_last_month = last_month,
           variable_start_year = start_year,
           variable_end_year = end_year) %>%
    relocate(register_id, contains("register_name"),
             variable_name, contains("description")) %>%
    relocate(ends_with("url"), .after = last_col()) %>%
    mutate(use_in_application = NA_character_,
           reason_for_use = NA_character_,
           .before = 1) %>%
    write_csv(here::here("data/dst-registers-with-variables.csv"),
              na = "")

# For misc checking -------------------------------------------------------

# leftover_registers <- needed_registers %>%
#     anti_join(register_names_table) %>%
#     pull(register_abbreviation)
#
# register_names_table %>%
#     filter(str_detect(register_abbreviation, "IND")) %>%
#     View()
#
# register_names_table %>%
#     filter(første_år < 1980) %>%
#     View()

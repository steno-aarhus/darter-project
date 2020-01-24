
library(tidyverse)
library(rvest)

dst_url <- "http://www.dst.dk/extranet/forskningvariabellister/Oversigt%20over%20registre.html"

dst_html <- read_html(dst_url)

tidy_table_names <- function(x) {
    x %>%
        str_replace_all("\\.", "_") %>%
        str_to_lower()
}

dst_table <- dst_html %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table() %>%
    as_tibble(.name_repair = "universal") %>%
    rename_all(tidy_table_names)

saveRDS(dst_table, here::here("resources/dst-table.Rds"))


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

# Functions ---------------------------------------------------------------

stop("To prevent accidental sourcing.")

# Scraping ----------------------------------------------------------------

# dst_documentation_url_extension <- "da/TilSalg/Forskningsservice/Dokumentation/hoejkvalitetsvariable"
dst_documentation_url_extension <- "extranet/forskningvariabellister/Oversigt%20over%20registre.html"

dst_host <- bow("https://www.dst.dk")

scraped_dst <- dst_host %>%
    nod(dst_documentation_url_extension) %>%
    scrape(content = "text/html; charset=windows-1252")

registers <- scraped_dst %>%
    html_node(".branch") %>%
    html_nodes("a")

registers_tbl <- tibble(
    register_name = html_text(registers)[-1:-2],
    register_link = unlist(html_attrs(registers))[-1:-2]
)


scraped_dst %>%
    html_node(".branch") %>%
    html_nodes(".table") %>%
    html_table() %>%
    purrr::pluck(1) %>%
    as_tibble()


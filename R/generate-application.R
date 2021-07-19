
fs::dir_ls(here::here("applications/dst/"), glob = "*.Rmd") |>
    purrr::map(rmarkdown::render)

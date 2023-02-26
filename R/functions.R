compare_current_vs_needed_dst_registers <-
    function(file_current = "data/dst-registers-with-variables.csv",
             file_needed = "data/needed-registers.csv") {

    current_dst_registers <- here::here(file_current) %>%
        readr::read_csv(col_types = readr::cols_only(register_id = readr::col_character())) %>%
        dplyr::pull(register_id) %>%
        base::unique()

    needed_registers <- here::here(file_needed) %>%
        readr::read_csv(
            col_types = readr::cols_only(
                register_id = readr::col_character(),
                owner = readr::col_character()
            )
        ) %>%
        dplyr::filter(owner == "dst") %>%
        stats::na.omit() %>%
        dplyr::pull(register_id)

    list(
        current = current_dst_registers,
        needed = needed_registers,
        difference = dplyr::setdiff(needed_registers, current_dst_registers)
    )
    }

signature_table <- function(name, position) {
  table_prep <- tribble(
    ~base, ~values,
    "Navn:", name,
    "Stilling:", position,
    "Dato:", "",
    "Underskrift:", ""
  )

  table_prep %>%
    flextable::flextable() %>%
    flextable::delete_part() %>%
    flextable::border_remove() %>%
    flextable::width(width = c(2, 4.5))
}

authorized_researchers_table <- function(data) {
  flextable::flextable(data) %>%
    flextable::border_inner() %>%
    flextable::border_outer() %>%
    flextable::bold(part = "header") %>%
    flextable::valign(valign = "top", part = "header") %>%
    flextable::bg(bg = "#d9d9d9", part = "header") %>%
    flextable::bg(i = c(1, 3), bg = "#f2f2f2") %>%
    flextable::bold(i = c(1, 3)) %>%
    flextable::merge_h_range(
      i = c(1, 3),
      j1 = 1,
      j2 = ncol(authorized_researchers)
    ) %>%
    flextable::fontsize(size = 9, part = "all") %>%
    {
      if (!knitr::is_html_output()) {
        flextable::font(., fontname = "Bitstream Vera Sans", part = "all")
      } else {
        .
      }
    }
}

as_reactable <- function(data) {
  reactable::reactable(
    data,
    defaultColDef = reactable::colDef(align = "left"),
    columns = list(
      Description = reactable::colDef(minWidth = 250),
      Register = reactable::colDef(
        filterInput = function(values, name) {
          htmltools::tags$select(
            # Set to undefined to clear the filter
            onchange = sprintf("Reactable.setFilter('registers-select', '%s', event.target.value || undefined)", name),
            # "All" has an empty value to clear the filter, and is the default option
            htmltools::tags$option(value = "", "All"),
            lapply(unique(values), htmltools::tags$option),
            "aria-label" = sprintf("Filter %s", name),
            style = "width: 100%; height: 28px;"
          )
        }
      )
    ),
    elementId = "registers-select",
    highlight = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    showPageSizeOptions = TRUE,
    showSortable = TRUE,
    showSortIcon = TRUE,
    theme = reactable::reactableTheme(
      searchInputStyle = list(width = "100%")
    )
  )
}

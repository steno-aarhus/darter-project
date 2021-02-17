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


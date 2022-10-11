format_dst_appendix_table <- function(data) {
    data %>%
        dplyr::select(Variables, Beskrivelse, År) %>%
        flextable::flextable() %>%
        flextable::add_header_row(values = c("Grundregisternavn",
                                             unique(data$register_id),
                                             "")) %>%
        flextable::border_remove() %>%
        flextable::hline_top(border = officer::fp_border(color = "black"),
                             part = "all") %>%
        flextable::hline_bottom(border = officer::fp_border(color = "black"),
                                part = "body") %>%
        flextable::bold(part = "header") %>%
        flextable::color(color = "white", part = "header") %>%
        flextable::valign(valign = "top", part = "header") %>%
        flextable::bg(bg = "#4f81bd", part = "header") %>%
        {if (!knitr::is_html_output()) {flextable::font(., fontname = "Times New Roman", part = "all")} else {.}} %>%
        flextable::fontsize(size = 12, part = "all") %>%
        flextable::width(width = 2.25) %>%
        # To see it interactively (not while running rmarkdown, check out:
        # https://github.com/yihui/knitr/issues/926)
        flextable::flextable_to_rmd(., text_after = "\\pagebreak")
}

format_non_dst_appendix_table <- function(data, caption = NULL, width = 2.25, is_atc = FALSE) {
    bold_atc <- function(tbl) {
        tbl %>%
            flextable::bold(
                i = ~str_detect(`ATC Code`, "^[A-N]$"),
                j = ~Description + `ATC Code`
            )
    }
    data %>%
        flextable::flextable() %>%
        flextable::border_remove() %>%
        flextable::hline_top(border = officer::fp_border(color = "black"),
                             part = "all") %>%
        flextable::hline_bottom(border = officer::fp_border(color = "black"),
                                part = "body") %>%
        {if (is_atc) {bold_atc(.)} else {.}} %>%
        flextable::bold(part = "header") %>%
        flextable::color(color = "white", part = "header") %>%
        flextable::valign(valign = "top", part = "header") %>%
        flextable::bg(bg = "#4f81bd", part = "header") %>%
        {if (!knitr::is_html_output()) {flextable::font(., fontname = "Times New Roman", part = "all")} else {.}} %>%
        flextable::fontsize(size = 12, part = "all") %>%
        flextable::width(width = width) %>%
        {if (!is.null(caption)) {flextable::set_caption(., caption = caption)} else {.}} %>%
        # To see it interactively (not while running rmarkdown, check out:
        # https://github.com/yihui/knitr/issues/926)
        flextable::flextable_to_rmd(., text_after = "\\pagebreak")
}

icd_as_table <- function(data, group, caption, width = c(1.5, 2.5, 1.75, 1.75)) {
    data %>%
        filter(parent_group == group) %>%
        select(-parent_group) %>%
        format_non_dst_appendix_table(caption = caption, width = width)
}

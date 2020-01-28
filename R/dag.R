
library(magrittr)

grviz_to_svg <- function(gv_file, .save = FALSE) {
    svg_output <- DiagrammeR::grViz(gv_file) %>%
        DiagrammeRsvg::export_svg() %>%
        charToRaw()

    svg_file <- fs::path_ext_set(gv_file, "svg") %>%
        fs::path_file()

    if (.save) {
        rsvg::rsvg_svg(svg_output,
                       file = here::here("images", svg_file))
        return(invisible())
    }
    return(svg_output)
}

grviz_to_png <- function(gv_file, height, width) {
    png_output <- grviz_to_svg(gv_file)

    png_file <- fs::path_ext_set(gv_file, "png") %>%
        fs::path_file()
    png_output %>%
        rsvg::rsvg_png(here::here("images", png_file),
                       height = height, width = width)

    return(invisible())
}

grviz_to_png(here::here("R/dag.gv"), height = 600, width = 1200)
grviz_to_svg(here::here("R/dag.gv"), .save = TRUE)

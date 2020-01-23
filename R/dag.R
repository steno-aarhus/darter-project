
library(magrittr)

grviz_to_svg <- function(gv_file) {
    svg_file <- stringr::str_replace(gv_file, "\\.gv$", ".svg")
    svg_output <- DiagrammeR::grViz(gv_file) %>%
        DiagrammeRsvg::export_svg() %>%
        charToRaw()

    rsvg::rsvg_svg(svg_output, file = svg_file)
    svg_output
}
grviz_to_png <- function(gv_file, height) {
    png_file <- stringr::str_replace(gv_file, "\\.gv$", ".png")
    png_output <- grviz_to_svg(gv_file)

    png_output %>%
        rsvg::rsvg_png(png_file, height = height)
}

grviz_to_png("_includes/fig-overview-bio.gv", height = 400)
grviz_to_png("_includes/fig-overview-wp.gv", height = 300)


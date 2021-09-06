#!/usr/bin/env Rscript
# Need this to fix a warning/error (at least for this version)
# Sys.setenv(RSTUDIO_PANDOC = '/usr/lib/rstudio/bin/pandoc')
cli::cli_alert_info("Starting to render the documents.")

# To fix a bug in distill
if (!interactive()) Sys.setenv("RSTUDIO_VERSION" = '1.4.1717')
# Build website
rmarkdown::render_site(encoding = "UTF-8", quiet = TRUE)
cli::cli_alert_success("Website has been built!")

# Build DST Word docs
rmarkdown::render("description.Rmd",
            output_format = rmarkdown::word_document(
                reference_docx = "resources/templates/description.docx"
            ),
            output_dir = here::here("public"),
            quiet = TRUE
)

rmarkdown::render("appendix.Rmd",
            output_format = rmarkdown::word_document(
                reference_docx = "resources/templates/appendix.docx"
            ),
            output_dir = here::here("public"),
            quiet = TRUE
)

cli::cli_alert_success("Word documents have been generated!")
cli::cli_alert_info("All documents have been rendered.")

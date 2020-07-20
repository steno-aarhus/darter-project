library(dplyr)
library(tidyr)

# Taken and modified from https://lazappi.id.au/post/2016-06-13-gantt-charts-in-r/
# Columns should be something like:
# task_name, priority (like "crit"),  status (like "active" or "done"), task_label, start_date, end_date

# Take a data.frame containing tasks and build a Mermaid string
tasks_to_string <- function(tasks_df, section_col = NULL) {
    if (is.null(section_col)) {
        tasks_df %>%
            glue::glue_data("  {task_name}: {priority}, {status}, {task_label}, {start_date}, {end_date}") %>%
            as.character() %>%
            stringr::str_remove_all("( |NA),") %>%
            stringr::str_c(collapse = "\n")
    } else {
        tasks_df %>%
            dplyr::group_split(.data[[section_col]]) %>%
            purrr::map_chr(~ {
                tasks <-
                    glue::glue_data(
                        .,
                        "  {task_name}: {priority}, {status}, {task_label}, {start_date}, {end_date}"
                    ) %>%
                    as.character() %>%
                    stringr::str_remove_all("( |NA),") %>%
                    stringr::str_c(collapse = "\n")
                stringr::str_c("\n  section ", unique(.[[section_col]]), "\n", tasks)
            })
    }
}

# Produce a Gantt chart from data.frame of tasks
# Adds the Mermaid header to the tasks string
build_gantt <- function(tasks_df, section_col = NULL) {
    gantt_string <- paste0("gantt", "\n",
                           "  dateformat YYYY-MM-DD", "\n",
                           "  title Timeline",
                           "\n\n",
                           tasks_to_string(tasks_df, section_col))

    gantt <- DiagrammeR::mermaid(gantt_string)

    gantt$x$config = list(ganttConfig = list(
        # Make sure the axis labels are formatted correctly
        axisFormatter = list(list(
            "%W", # New data format
            htmlwidgets::JS('function(d){ return d}') # Select dates to format
        ))
    ))

    return(gantt)
}

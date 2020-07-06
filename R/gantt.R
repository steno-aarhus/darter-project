library(dplyr)
library(tidyr)
library(stringr)

# Taken and modified from https://lazappi.id.au/post/2016-06-13-gantt-charts-in-r/

# Take a data.frame containing tasks and build a Mermaid string
# Columns should be something like:
# task name, priority (like "crit"),  status (like "active" or "done"), label, start, end
tasks_to_string <- function(tasks_df) {
    task_list <- tasks_df %>%
        dplyr::arrange(deliverable) %>%
        dplyr::group_split(deliverable) %>%
        purrr::map_chr(
            ~ glue::glue_data(
                .,
                "  {task_name}: {priority}, {status}, {task_label}, {start_date}, {end_date}"
            ) %>%
                stringr::str_remove_all(" NA,") %>%
                stringr::str_flatten("\n")
        )

    glue::glue("  section {sort(unique(tasks_df$deliverable))}\n{task_list}\n\n") %>%
        stringr::str_flatten("\n")
}

# Produces a Gantt chart from a data frame of tasks
create_gantt_chart <- function(tasks_df, title = "Gantt Chart") {

    tasks_string <- tasks_to_string(tasks_df)
    gantt_string <- glue::glue("
    gantt
      dateFormat YYYY-MM-DD
      title {title}

      {tasks_string}
    ")

    gantt <- DiagrammeR::mermaid(gantt_string)

    gantt$x$config <- list(ganttConfig = list(
        # Make sure the axis labels are formatted correctly
        axisFormatter = list(list(
            "%b", # New data format
            htmlwidgets::JS('function(d){ return d}') # Select dates to format
        ))
    ))

    return(gantt)
}

library(dplyr)
library(tidyr)

# Taken and modified from https://lazappi.id.au/post/2016-06-13-gantt-charts-in-r/
# Columns should be something like:
# task name, priority (like "crit"),  status (like "active" or "done"), label, start, end

# Take a data.frame containing tasks and build a Mermaid string
tasks_to_string <- function(tasks_df) {

    tasks_list <- tasks_df %>%
        dplyr::group_split(LinkedDeliverable) %>%
        tidyr::unite(col = "task_string_prep",
                     task_name, priority,
                     sep = ": ") %>%
        tidyr::unite(col = "task_item_string",
                     task_item_string_prep, status, task_label, start_date, end_date,
                     sep = ", ") %>%
        dplyr::pull(task_item_string) %>%
        paste(collapse = "\n") %>%
        stringr::str_remove_all(" ,", "")


    strings <- sapply(names(tasks.list),
                      function(section) {
                          tasks.list[[section]] %>%
                              dplyr::select(-Section) %>%
                              tidyr::unite(Part1, Task, Priority,
                                           sep = ": ") %>%
                              tidyr::unite(String, Part1, Status, Name, Start,
                                           End, sep = ", ") %>%
                              magrittr::use_series("String") %>%
                              paste(collapse = "\n") %>%
                              gsub(" ,", "", .) # Remove empty columns
                      }
    )

    string <- ""

    for(section in names(strings)) {
        string <- paste0(string, "\n",
                         "section ", section, "\n",
                         strings[section],
                         "\n")
    }

    return(string)
}

# Produce a Gantt chart from data.frame of tasks
# Adds the Mermaid header to the tasks string
buildGantt <- function(tasks) {

    gantt.string <- paste0("gantt", "\n",
                           "dateformat YYYY-MM-DD", "\n",
                           "title My Gantt Chart",
                           "\n")

    gantt.string <- paste0(gantt.string, tasks2string(tasks))

    gantt <- DiagrammeR::mermaid(gantt.string)

    gantt$x$config = list(ganttConfig = list(
        # Make sure the axis labels are formatted correctly
        axisFormatter = list(list(
            "%m-%y", # New data format
            htmlwidgets::JS('function(d){ return d}') # Select dates to format
        ))
    ))

    return(gantt)
}

# Read a file and return a Gantt chart
buildGanttFromFile <- function(tasks.file, format = c("csv", "xlsx")) {

    format <- match.arg(format)

    switch(format,
           csv = {
               tasks <- read.csv(tasks.file, stringsAsFactors = FALSE)
           },
           xlsx = {
               tasks <- gdata::read.xls(tasks.file)
           })

    return(buildGantt(tasks))
}

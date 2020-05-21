library("magrittr")

# Take a data.frame containing tasks and build a Mermaid string
tasks2string <- function(tasks) {

    tasks.list <- split(tasks,
                        factor(tasks$Section, levels = unique(tasks$Section)))

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

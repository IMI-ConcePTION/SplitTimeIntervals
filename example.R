source("SplitTimeIntervals.R")
library(data.table)

##############
example_1 <- fread("examples/input/input.csv")
example_1 <- example_1[, start_date := as.Date(start_date)]
example_1 <- example_1[, end_date := as.Date(end_date)]
example_1 <- example_1[, intermediate_date := as.Date(intermediate_date)]

example_1 <- example_1[, person_id := id]

DT_splitted <- SplitTimeIntervals(dataset = example_1,
                                  id_SplitTimeIntervals = "person_id",
                                  start_date = "start_date",
                                  end_date = "end_date",
                                  id_vars = c(),
                                  start_intervals = list("intermediate_date") ,
                                  label_of_split_records = c("first", "second"),
                                  labelvar = "interval"
                                  )

##############
example_2 <- fread("examples/input/example_2.csv")

example_2 <- example_2[, date_split_1 := as.Date(date_split_1)]
example_2 <- example_2[, date_split_2 := as.Date(date_split_2)]
example_2 <- example_2[, date_split_3 := as.Date(date_split_3)]
example_2 <- example_2[, start_period  := as.Date(start_period )]
example_2 <- example_2[, end_period := as.Date(end_period)]


DT <- SplitTimeIntervals(dataset = example_2,
                         id_SplitTimeIntervals = "id",
                         start_date = "start_period",
                         end_date = "end_period",
                         start_intervals = list("date_split_1", "date_split_2", "date_split_3"),
                         label_of_split_records = c("first", "second", "third", "fourth"),
                         labelvar = "Interval_variable_name",
                         include_NA_intervals = TRUE
                         )

source("SplitTimeIntervals.R")
library(data.table)

##############
example_1 <- fread("examples/input/input.csv")

DT_splitted <- SplitTimeIntervals(dataset = example_1,
                                  id_SplitTimeIntervals = "id",
                                  start_date = "start_date",
                                  end_date = "end_date",
                                  id_vars = c(),
                                  start_intervals = list("intermediate_date"),
                                  label_of_split_records = c("first", "second"),
                                  labelvar = "interval")

##############
example_2 <- fread("examples/input/example_2.csv")

DT <- SplitTimeIntervals(dataset = example_2,
                         id_SplitTimeIntervals = "id",
                         start_date = "start_period",
                         end_date = "end_period",
                         start_intervals = list("date_split_1", "date_split_2", "date_split_3"),
                         label_of_split_records = c("first", "second", "third", "fourth"),
                         labelvar = "Interval_variable_name",
                         include_NA_intervals = TRUE)

##############
example_3 <- fread("examples/input/example_3.csv")

DT <- SplitTimeIntervals(dataset = example_3,
                         id_SplitTimeIntervals = "id",
                         start_date = "start_period",
                         end_date = "end_period",
                         id_vars = c("age", "sex"),
                         start_intervals = list("date_split_1", "date_split_2", "date_split_3"),
                         label_of_split_records = c("first", "second", "third", "fourth"),
                         labelvar = "Interval_variable_name",
                         include_NA_intervals = TRUE)

##############
dataset <- fread("examples/input/input.csv")
dataset <- dataset[, start_date := as.Date(start_date)]
dataset <- dataset[, end_date := as.Date(end_date)]
dataset <- dataset[, intermediate_date := as.Date(intermediate_date)]

DT_splitted <- SlitTimeIntervals(dataset = dataset,
                                 id = "id",
                                 start_date = "start_date",
                                 end_date = "end_date",
                                 start_intervals = list("intermediate_date") ,
                                 label_of_split_records = "interval",
                                 labelvar = "interval",
                                 include_end)


##############

dataset <- fread("examples/input/example_2.csv")

dataset <- dataset[, date_split_1 := as.Date(date_split_1)]
dataset <- dataset[, date_split_2 := as.Date(date_split_2)]
dataset <- dataset[, date_split_3 := as.Date(date_split_3)]
dataset <- dataset[, start_period  := as.Date(start_period )]
dataset <- dataset[, end_period := as.Date(end_period)]


DT <- SlitTimeIntervals(dataset = dataset,
                        id = "id",
                        start_date = "start_period",
                        end_date = "end_period",
                        start_intervals = list("date_split_1", "date_split_2", "date_split_3") ,
                        label_of_split_records = "interval",
                        labelvar = "interval",
                        include_end)

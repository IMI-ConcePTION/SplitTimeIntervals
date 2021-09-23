#' 'SlitTimeIntervals'
#' 
#' 
#' @param dataset (str): name of the data.table input dataset
#' @param id_SplitTimeIntervals (str): name of the variable containing the identifier of the record
#' @param start_date (date): name of the variable containing the start date of record; the variable must be a date variable
#' @param end_date (date): name of the variable containing the end date of record; the variable must be a date variable
#' @param id_vars (vector): a vector containing all the constant variables (excluding id, start_date and end_date) 
#' @param start_intervals (list of strings): list of variable names, each containing a date
#' @param label_of_split_records (list of strings): list of labels that each new record will contain
#' @param labelvar (str): name of the variable containing the labels of the new records
#' @param include_end (bool): (optional) default FALSE: if TRUE, the date of the 'start_interval' is included in the previous interval, instead of being inlcuded in the new one
#' @param include_NA_intervals (bool): (optional) default FALSE: if TRUE, the NA intervals are included 



SplitTimeIntervals <- function(dataset,
                               id_SplitTimeIntervals,
                               start_date,
                               end_date,
                               id_vars = c(),
                               start_intervals,
                               label_of_split_records = NULL,
                               labelvar = "interval",
                               include_end = TRUE,
                               include_NA_intervals = FALSE
                              ){
  #libraries
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  if (!require("lubridate")) install.packages("lubridate")
  library(lubridate)
  
  ## check: vars
  for (var in c(id_vars, start_intervals, start_date, end_date, id_SplitTimeIntervals)) {
    if(!(var %in% names(dataset))){
      stop(paste0("The variable ", var, " is not present in the dataset"))
    }
  }
  
  ## check: label_of_split_records
  if (!is.null(label_of_split_records)){
    if (length(label_of_split_records) < length(start_intervals)+1){
      stop("The length of label_of_split_records does not match the number of start_intervals")
    }
  }

  # check: date format
  for (date in c(start_date, end_date, start_intervals)) {
    if(!is.Date(dataset[, get(date)])){
      stop("One or more date variable are not in date format")
    }else{
      setnames(dataset, date, "date_tmp")
      dataset <- dataset[, date_tmp:= as.Date(date_tmp)]
      setnames(dataset, "date_tmp", date)
    }
  }
  
  # check: chronological order
  check_dataset <- copy(dataset)
  check_dataset <- check_dataset[, check := 0]
  
  for (date in start_intervals) {
    setnames(check_dataset, 
             date,
             "tmp_date")
    
    check_dataset <- check_dataset[is.na(tmp_date), tmp_date := as.Date("9999-12-31")]
    
    setnames(check_dataset, 
             "tmp_date",
             date)
  }
  
  setnames(check_dataset, 
           c(end_date, start_date), 
           c("end_date", "start_date"))
  
  check_dataset <- check_dataset[end_date < start_date, check := 1]
  check_dataset <- check_dataset[get(start_intervals[[1]]) < start_date, check := 1]
  check_dataset <- check_dataset[end_date < get(start_intervals[[length(start_intervals)]]) &
                                   get(start_intervals[[length(start_intervals)]]) !=  as.Date("9999-12-31"),
                                 check := 1]
  
  if(length(start_intervals) > 1){
    for (i in seq(2, length(start_intervals))) {
      check_dataset <- check_dataset[get(start_intervals[[i-1]]) > get(start_intervals[[i]]) | 
                                       (get(start_intervals[[i]]) > end_date &
                                        get(start_intervals[[i]]) !=  as.Date("9999-12-31")), 
                                     check := 1]
    }
  }
  
  if (max(check_dataset[, check]) == 1){
    stop("Dates are not in chronological order ")
  }
  
  ## Function 
  
  dataset <- dataset[, splitted := 0]
  number_of_intervals <- length(start_intervals) + 1
  i = 1 
  
  setnames(dataset, 
           c(start_date, end_date),
           c("start_date", "end_date"))
  
  dataset <- dataset[, period_1_start := start_date]
  
  if (include_end){
    for (date in start_intervals) {
      
      dataset <- dataset[is.na(get(date)) & splitted == 0, 
                         `:=`(period_tmp1_end = end_date,
                              splitted = 1 )]
      
      dataset <- dataset[!is.na(get(date)) & splitted == 0 , 
                         `:=`(period_tmp1_end = get(date) - 1,
                              period_tmp2_start = get(date))]
      
      setnames(dataset, 
               c("period_tmp1_end", "period_tmp2_start"),
               c(paste0("period_", i, "_end"), paste0("period_", i + 1, "_start")))
      
      i = i + 1
    }
  }else{
    for (date in start_intervals) {
      
      dataset <- dataset[is.na(get(date)) & splitted == 0, 
                         `:=`(period_tmp1_end = end_date,
                              splitted = 1 )]
      
      dataset <- dataset[!is.na(get(date)) & splitted == 0 , 
                         `:=`(period_tmp1_end = get(date) ,
                              period_tmp2_start = get(date) + 1)]
      
      setnames(dataset, 
               c("period_tmp1_end", "period_tmp2_start"),
               c(paste0("period_", i, "_end"), paste0("period_", i + 1, "_start")))
      
      i = i + 1
    }
  }
  
  dataset <- dataset[splitted == 0, `:=`(period_tmp_end = end_date)]
  
  setnames(dataset, 
           c("period_tmp_end"),
           c(paste0("period_", i, "_end")))
  
  DT_splitted = melt(dataset, 
                     id.vars = c(id_SplitTimeIntervals, "start_date", "end_date", id_vars),
                     measure.vars = list(paste0("period_", seq(1, i), "_start"), 
                                         paste0("period_", seq(1, i), "_end")))

  DT_splitted <- DT_splitted[order(get(id_SplitTimeIntervals))]
  
  ## Rename interval
  DT_splitted[, variable:= as.character(variable)]
  
  if (!is.null(label_of_split_records)){
    for (j in seq(1:i)) {
      DT_splitted[variable == as.character(j), variable:= label_of_split_records[[j]]]
    }
  }
  
  if(!include_NA_intervals){
    DT_splitted <- DT_splitted[!is.na(value1)]
  }
  
  setnames(DT_splitted, 
           c("variable", "value1", "value2", "start_date", "end_date"),
           c(labelvar, "start", "end", start_date, end_date)) 
  
  return(DT_splitted)
}

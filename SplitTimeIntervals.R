#' 'SlitTimeIntervals'
#' 
#' 
#' @param dataset (str): name of the data.table input dataset
#' @param id (str): name of the variable containing the identifier of the record
#' @param start_date (date): name of the variable containing the start date of record; the variable must be a date variable
#' @param end_date (date): name of the variable containing the end date of record; the variable must be a date variable
#' @param start_intervals (list of strings): list of variable names, each containing a date
#' @param label_of_split_records (list of strings): list of labels that each new record will contain
#' @param labelvar (str): name of the variable containing the labels of the new records
#' @param include_end (bool): (optional) default FALSE: if TRUE, the date of the 'start_interval' is included in the previous interval, instead of being inlcuded in the new one
#' @param include_NA_intervals (bool): (optional) default FALSE: if TRUE, the NA intervals are included 



SlitTimeIntervals <- function(dataset,
                              id,
                              start_date,
                              end_date,
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
  
  # check
  for (date in c(start_date, end_date, label_of_split_records)) {
    if(!is.Date(dataset[, get(date)])){
      stop(("Some variable are not in date format"))
    }
  }
  
  
  
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
                     id.vars = c("id", "start_date", "end_date"),
                     measure.vars = list(paste0("period_", seq(1, i), "_start"), 
                                         paste0("period_", seq(1, i), "_end")))

  DT_splitted <- DT_splitted[order(id)]
  
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
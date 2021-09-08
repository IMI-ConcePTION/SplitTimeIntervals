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




SlitTimeIntervals <- function(dataset,
                              id,
                              start_date,
                              end_date,
                              start_intervals ,
                              label_of_split_records,
                              labelvar = "interval",
                              include_end
                              ){
  #libraries
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  
  # Melting 
  dataset <- dataset[, splitted := 0]
  number_of_intervals <- length(start_intervals) + 1
  i = 1 
  
  setnames(dataset, 
           c(start_date, end_date),
           c("start_date", "end_date"))
  
  dataset <- dataset[, period_1_start := start_date]
  
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
  
  dataset <- dataset[splitted == 0, `:=`(period_tmp_end = end_date)]
  
  setnames(dataset, 
           c("period_tmp_end"),
           c(paste0("period_", i, "_end")))
  
  
  
  DT_splitted = melt(dataset, 
                     id.vars = c("id", "start_date", "end_date"),
                     measure.vars = list(paste0("period_", seq(1, i), "_start"), 
                                         paste0("period_", seq(1, i), "_end")))
  setnames(DT_splitted, 
           c("variable", "value1", "value2"),
           c(label_of_split_records, "start", "end"))
  
  
  DT_splitted <- DT_splitted[order(id)]
  
  return(DT_splitted)
}
-------------------------------
# example 1

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

setwd("..")
setwd("..")
dirbase<-getwd()
#load function
source(paste0(dirbase,"/SplitTimeIntervals_v1.R"))

# load data.table
if (!require("data.table")) install.packages("data.table")
library(data.table)


#load input
input <-fread(paste0(thisdir,"/input/input.csv"), sep = ",")


#USE 

output <- SplitTimeIntervals(dataset = input, 
                             id = "id",
                             start_date = "start_date",
                             end_date = "end_date",
                             start_intervals = c("intermediate_date"),
                             label_of_split_records = c("first","second"),
                             labelvar = "label"
                              )

View(output)
fwrite(output,file=paste0(thisdir,"/input/output.csv"))

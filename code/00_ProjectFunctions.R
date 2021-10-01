##### Functions I created for the project #####
## AF:updated 01 Feb 2021

options(max.print=1000000)
require(dplyr)


# values_by_column fun ----------------------------------------------------
# To extract unique values in each col of the dataset + counts by category
## input_dataset -> input R dataframe
## output_dir -> output folder where to store the text file with values by table col
## output_name -> name to give to the output txt file

values_by_column<-function(input_dataset, output_dir, output_name){
  # create a txt file with distinct values by column
  sink(paste0(output_dir,output_name,".txt"), append = F)
  
  #short file intro
  print(paste("Time file creation:", Sys.time(),sep=" "))
  print(paste("Distinct values by column for dataset:",input_dataset, sep=" "))
  cat("\n")
  
  #extract unique values by each col and order the values (ascending order)
  #use "get" to refer to a variable by using a string 
  output<-lapply(get(input_dataset),function(x){sort(unique(x))}) 
  print(output)
  sink()
  
  # create a txt file with N of distinct values by column
  sink(paste0(output_dir,output_name,"_counts.txt"), append = F)
  
  #short file intro
  print(paste("N values for each category by column for dataset:",input_dataset, sep=" "))
  cat("\n")
  
  #extract unique values by each col and order the values (ascending order)
  #use "get" to refer to a variable by using a string 
  output<-lapply(get(input_dataset),table) 
  print(output)
  sink()
  
  # create a txt file with N of blank cells by column
  sink(paste0(output_dir,output_name,"_blanks.txt"), append = F)
  
  #short file intro
  print(paste("N blanks for each category by column for dataset:",input_dataset, sep=" "))
  cat("\n")
  
  #extract unique values by each col and order the values (ascending order)
  #use "get" to refer to a variable by using a string 
  output<-lapply(get(input_dataset), function(x) {sum(is.na(x))})
  print(output)
  sink()
}


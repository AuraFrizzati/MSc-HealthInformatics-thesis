##### Functions I created for the project #####
## AF:updated 01 Feb 2021

options(max.print=1000000)
require(dplyr)
#.........................................................................................
#.........................................................................................

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
#.........................................................................................
#.........................................................................................

# DescriptiveCatBasic fun ----------------------------------------------------
#### Extract Descriptive stats for categorical main predictors (OHS only) 

DescriptiveCatBasic<-function(input_dataset, output_table){
  DescriptiveCatBasicTable<- rbind(  
    # sample size
    input_dataset%>%
      count()  %>%
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      mutate(VAR = "SAMPLE SIZE") %>% 
      mutate(VALUE_COL = "Total") %>%
      select(c(VAR,VALUE_COL), everything()) #to change columns' position
    
    ,
    
    # gender 
    input_dataset%>%count(SEX) %>%
      mutate(VAR = "GENDER") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = SEX) %>% 
      mutate(VALUE_COL=ifelse(VALUE_COL==1, "Males","Females")) %>%  # SEX = 1 ->MALE , SEX = 2 -> FEMALEs
      select(c(VAR,VALUE_COL), everything()) #to change columns' position 
    ,
    
    # ageband 
    input_dataset%>%count(AGEBAND) %>%
      mutate(VAR = "Age") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = AGEBAND) %>% 
      #mutate(VALUE_COL=ifelse(VALUE_COL==1, "Males","Females")) %>%  # SEX = 1 ->MALE , SEX = 2 -> FEMALEs
      select(c(VAR,VALUE_COL), everything()) #to change columns' position 
    
    ,
    
    # REVISION 
    input_dataset%>%count(REVISION) %>%
      mutate(VAR = "REVISION") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = REVISION) %>% 
      mutate(VALUE_COL=ifelse(VALUE_COL==1, "YES","NO")) %>%  # REVISION = 0 -> NO , REVISION = 1 -> YES
      select(c(VAR,VALUE_COL), everything()) #to change columns' position 
    
    ,
    
    # OHS PREOP PAIN
    input_dataset%>%count(OHS_PREOP_PAIN) %>%
      mutate(VAR = "OHS PREOP PAIN") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_PAIN) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position  
    ,
    
    # OHS PREOP SUDDENPAIN
    input_dataset%>%count(OHS_PREOP_SUDDENPAIN) %>%
      mutate(VAR = "OHS PREOP SUDDENPAIN") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_SUDDENPAIN) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position  
    ,
    
    # OHS PREOP NIGHTPAIN
    input_dataset%>%count(OHS_PREOP_NIGHTPAIN) %>%
      mutate(VAR = "OHS PREOP NIGHTPAIN") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_NIGHTPAIN) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position    
    ,
    
    # OHS PREOP WASHING
    input_dataset%>%count(OHS_PREOP_WASHING) %>%
      mutate(VAR = "OHS PREOP WASHING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_WASHING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position      
    ,
    
    # OHS PREOP TRANSPORT
    input_dataset%>%count(OHS_PREOP_TRANSPORT) %>%
      mutate(VAR = "OHS PREOP TRANSPORT") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_TRANSPORT) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position    
    ,
    
    # OHS PREOP DRESSING
    input_dataset%>%count(OHS_PREOP_DRESSING) %>%
      mutate(VAR = "OHS PREOP DRESSING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_DRESSING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position     
    ,
    
    # OHS PREOP SHOPPING
    input_dataset%>%count(OHS_PREOP_SHOPPING) %>%
      mutate(VAR = "OHS PREOP SHOPPING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_SHOPPING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position    
    ,
    
    # OHS PREOP WALKING
    input_dataset%>%count(OHS_PREOP_WALKING) %>%
      mutate(VAR = "OHS PREOP WALKING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_WALKING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position   
    ,
    
    # OHS PREOP LIMPING
    input_dataset%>%count(OHS_PREOP_LIMPING) %>%
      mutate(VAR = "OHS PREOP LIMPING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_LIMPING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position   
    ,
    
    # OHS PREOP STAIRS
    input_dataset%>%count(OHS_PREOP_STAIRS) %>%
      mutate(VAR = "OHS PREOP STAIRS") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_STAIRS) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position    
    ,
    
    # OHS PREOP STANDING
    input_dataset%>%count(OHS_PREOP_STANDING) %>%
      mutate(VAR = "OHS PREOP STANDING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_STANDING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position   
    ,
    
    # OHS PREOP WORK
    input_dataset%>%count(OHS_PREOP_WORK) %>%
      mutate(VAR = "OHS PREOP WORK") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_PREOP_WORK) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position     
    ,
    
    # OHS MCID
    input_dataset%>%count(OHS_MCID) %>%
      mutate(VAR = "OHS MCID") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OHS_MCID) %>% 
      mutate(VALUE_COL=ifelse(VALUE_COL==1, "YES","NO")) %>%  # OHS_MCID = 1 -> YES , OHS_MCID = 0 -> NO
      select(c(VAR,VALUE_COL), everything()) #to change columns' position       
    ,
    
    # EQ-VAS MCID
    input_dataset%>%count(VAS_MCID) %>%
      mutate(VAR = "EQ-VAS MCID") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = VAS_MCID) %>% 
      mutate(VALUE_COL=ifelse(VALUE_COL==1, "YES","NO")) %>%  # VAS_MCID = 1 -> YES , VAS_MCID = 0 -> NO
      select(c(VAR,VALUE_COL), everything()) #to change columns' position    
  )
  
  # assign the output_df name given as a function argument (GlobalEnv so it is accessible outside of the function) 
  assign(deparse(substitute(output_table)), DescriptiveCatBasicTable, envir=.GlobalEnv)
}
#.........................................................................................
#.........................................................................................

# DescriptiveCont2 fun ----------------------------------------------------
#### Extract Descriptive stats for continuous main predictors (OHS only), median and IQR 
DescriptiveCont2<-function(input_dataset, output_table){
  DescriptiveContTable<- rbind(
    
    ##OHS PRE-OP
    inner_join(
      #OHS preop median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(OHS_PREOP_TOTSCORE,0.5),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="OHS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #OHS preop Q1
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(OHS_PREOP_TOTSCORE,0.25),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="OHS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #OHS preop Q3
      input_dataset%>%
        summarise(
          Q3 = round(quantile(OHS_PREOP_TOTSCORE,0.75),2) )%>%
        mutate(VALUE_COL="PreOperative")%>%
        mutate(VARIABLE="OHS Total Score")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    ,
    ##OHS POST-OP
    inner_join(
      #OHS postop median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(OHS_POSTOP_TOTSCORE,0.5),2) )%>%
                   mutate(VALUE_COL="PostOperative")%>%
                   mutate(VARIABLE="OHS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #OHS postop Q1
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(OHS_POSTOP_TOTSCORE, 0.25),2) )%>%
                   mutate(VALUE_COL="PostOperative")%>%
                   mutate(VARIABLE="OHS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #OHS postop Q3
      input_dataset%>%
        summarise(
          Q3 = round(quantile(OHS_POSTOP_TOTSCORE, 0.75),2) )%>%
        mutate(VALUE_COL="PostOperative")%>%
        mutate(VARIABLE="OHS Total Score")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    ,
    
    ##OHS_TOTSCORE.diff
    inner_join(
      #OHS_TOTSCORE.diff median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(OHS_TOTSCORE.diff, 0.5),2) )%>%
                   mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
                   mutate(VARIABLE="OHS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #OHS_TOTSCORE.diff Q1
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(OHS_TOTSCORE.diff,0.25),2) )%>%
                   mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
                   mutate(VARIABLE="OHS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #OHS_TOTSCORE.diff Q3
      input_dataset%>%
        summarise(
          Q3 = round(quantile(OHS_TOTSCORE.diff,0.75),2) )%>%
        mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
        mutate(VARIABLE="OHS Total Score")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    
    ,
    
    ##EQ-VAS PRE-OP
    inner_join(
      #VAS preop Median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(EQ5D_PREOP_VAS, 0.5),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 #VAS preop Q1 
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(EQ5D_PREOP_VAS, 0.25),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      #VAS preop Q3 
      input_dataset%>%
        summarise(
          Q3 = round(quantile(EQ5D_PREOP_VAS, 0.75),2) )%>%
        mutate(VALUE_COL="PreOperative")%>%
        mutate(VARIABLE="EQ-VAS")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    ,
    
    ##EQ-VAS POST-OP
    inner_join(
      #VAS postop Median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(EQ5D_POSTOP_VAS, 0.5),2) )%>%
                   mutate(VALUE_COL="PostOperative")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 #VAS postop Q1
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(EQ5D_POSTOP_VAS, 0.25),2) )%>%
                   mutate(VALUE_COL="PostOperative")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #VAS postop Q3
      input_dataset%>%
        summarise(
          Q3 = round(quantile(EQ5D_POSTOP_VAS, 0.75),2) )%>%
        mutate(VALUE_COL="PostOperative")%>%
        mutate(VARIABLE="EQ-VAS")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    ,
    
    
    ##VAS_TOTSCORE.diff
    inner_join(
      #VAS_TOTSCORE.diff median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(VAS_TOTSCORE.diff,0.5),2) )%>%
                   mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #VAS_TOTSCORE.diff Q1 
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(VAS_TOTSCORE.diff, 0.25),2) )%>%
                   mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #VAS_TOTSCORE.diff Q3 
      input_dataset%>%
        summarise(
          Q3 = round(quantile(VAS_TOTSCORE.diff, 0.75),2) )%>%
        mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
        mutate(VARIABLE="EQ-VAS")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    ,
    
    ##EQ5D PRE-OP INDEX 
    inner_join(
      #EQ5D PRE-OP INDEX Median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(EQ5D_PREOP_INDEX, 0.5),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="EQ5D INDEX")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #EQ5D PRE-OP INDEX Q1 
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(EQ5D_PREOP_INDEX, 0.25),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="EQ5D INDEX")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #EQ5D PRE-OP INDEX Q3 
      input_dataset%>%
        summarise(
          Q3 = round(quantile(EQ5D_PREOP_INDEX, 0.75),2) )%>%
        mutate(VALUE_COL="PreOperative")%>%
        mutate(VARIABLE="EQ5D INDEX")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    
  )
  
  # assign the output_df name given as a function argument (GlobalEnv so it is accessible outside of the function) 
  assign(deparse(substitute(output_table)), DescriptiveContTable, envir=.GlobalEnv)
}

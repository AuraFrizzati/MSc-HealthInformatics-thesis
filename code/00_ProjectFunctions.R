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

    if(sum(is.na(input_dataset$OHS_POSTOP_TOTSCORE)) == 0){
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
    }
    ,
    
    ##OHS_TOTSCORE.diff
    if(sum(is.na(input_dataset$OHS_TOTSCORE.diff)) == 0){
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
    }
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
#.........................................................................................
#.........................................................................................

# ExtractMetrics fun ----------------------------------------------------
###### Function to extract model (training + testing) discrimination metrics
#### Parameters:

### (I) thresholder.output --> object obtained using caret function 
###     thresholder(trained_model,threshold = seq(0,1,0.05),final = TRUE,statistics = "all")

### (II) trained.model --> model trained with caret

### (III) test_set.model_probabilities --> predicted probabilities on test model
### calculated using the trained model. The probabilities are calculated using caret function:
### predict(trained_model, newdata = test_set, type = "prob")

### (IV) test_set --> the dataset on which the trained model is tested

### (V) OutputVar --> the output variable of interest (either "OHS_MCID" or "VAS_MCID")

### (VI) test_set.confusion_matrix --> the confusion matrix object built predicting the
### test set using the trained model. The confusion matrix is calculated using caret function:
### confusionMatrix(data = test_set_predicted_outcome_classes,
### reference = test_set_outcome_reference_column, positive = "YES")

ExtractMetrics <- function(
  thresholder.output,
  trained.model,
  test_set.model_probabilities,
  test_set,
  OutputVar,
  test_set.confusion_matrix,
  algorithm
){
  ##[0] Load relevant libraries
  library(caret); library(ROCR); library(tidyr); library(dplyr)
  
  ## [1] Create the training metric table
  values_ths<-c("prob_threshold", "Sensitivity", "Specificity", "J","Balanced Accuracy", 
                "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1", 
                "Prevalence", "Detection Rate", "Detection Prevalence", "Accuracy", 
                "Kappa", "Dist")
  metrics.training<-head(thresholder.output[order(thresholder.output$J, decreasing = TRUE), 
                                            values_ths],1)
  metrics.training<-metrics.training[,c("prob_threshold","Sensitivity", "Specificity",
                                        "Pos Pred Value", "Neg Pred Value", "J", "Balanced Accuracy", 
                                        "F1")]
  metrics.training$AUROC<-head(trained.model$results[order(trained.model$results$ROC, 
                                                           decreasing = TRUE),],1)$ROC
  row.names(metrics.training)<-NULL
  names(metrics.training)<-c("prob_threshold", "Sensitivity", "Specificity",
                             "PPV", "NPV", "J", "BalancedAcc", "F1", "AUROC")
  
  metrics.training$PHASE<-"TRAINING"
  #print(metrics.training)
  
  ## [2] Create the test metric table
  ROCR_prediction <- prediction(predictions = test_set.model_probabilities$YES, 
                                labels = test_set[,OutputVar])
  #print(ROCR_prediction)
  metrics.test<-as.data.frame(as.numeric((performance(ROCR_prediction, measure = "auc")@y.values)))
  metrics.test$Sensitivity<-as.data.frame(test_set.confusion_matrix$byClass)["Sensitivity",]
  metrics.test$Specificity<-as.data.frame(test_set.confusion_matrix$byClass)["Specificity",]
  metrics.test$PPV<-as.data.frame(test_set.confusion_matrix$byClass)["Pos Pred Value",]
  metrics.test$NPV<-as.data.frame(test_set.confusion_matrix$byClass)["Neg Pred Value",]
  metrics.test$F1<-as.data.frame(test_set.confusion_matrix$byClass)["F1",]
  metrics.test$BalancedAcc<-as.data.frame(test_set.confusion_matrix$byClass)["Balanced Accuracy",]
  
  metrics.test$F1<-test_set.confusion_matrix$table[1,1]/
    (test_set.confusion_matrix$table[1,1] +
       0.5*(test_set.confusion_matrix$table[1,2]+
              test_set.confusion_matrix$table[2,1]))
  
  metrics.test$J<-(test_set.confusion_matrix$table[1,1]/
                     (test_set.confusion_matrix$table[1,1]+
                        test_set.confusion_matrix$table[2,1]))+
    (test_set.confusion_matrix$table[2,2]/
       (test_set.confusion_matrix$table[2,2]+
          test_set.confusion_matrix$table[1,2]))-1
  
  metrics.test$prob_threshold<-NA
  
  names(metrics.test)<-c("AUROC", "Sensitivity", "Specificity",
                         "PPV", "NPV", "F1","BalancedAcc", "J", "prob_threshold")
  
  
  metrics.test<-metrics.test[,c("prob_threshold", "Sensitivity", "Specificity",
                                "PPV", "NPV", "J", "BalancedAcc", "F1", "AUROC")]
  
  metrics.test$PHASE<-"TEST"
  
  ## [3] Union of training + test metric table
  metrics.final<-rbind(metrics.training,metrics.test)
  
  metrics.final<-metrics.final[,c("PHASE","prob_threshold", "Sensitivity", "Specificity",
                                  "PPV", "NPV", "J", "BalancedAcc", "F1", "AUROC")]
  
  metrics.final<-metrics.final %>% 
    gather(Metric, !!algorithm, c("prob_threshold", "Sensitivity", 
                                  "Specificity","PPV", "NPV", "AUROC", "J", "BalancedAcc", 
                                  "F1")) %>%
    arrange(desc(PHASE))%>% 
    mutate(!!algorithm := sprintf("%0.2f", get(!!algorithm)))%>% # round to two decimal points
    slice(-10) # remove prob_threshold row for test (NULL)
  
  ## [4] Rename final table
  
  return(metrics.final)
  
}
#.........................................................................................
#.........................................................................................

# DescriptiveCatBasic.OKS fun ----------------------------------------------------
#### DESCRIPTIVE: CATEGORICAL - BASIC PREDICTORS (KNEE DATASET)
DescriptiveCatBasic.OKS<-function(input_dataset, output_table){
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
    
    # OKS PREOP PAIN
    input_dataset%>%count(OKS_PREOP_PAIN) %>%
      mutate(VAR = "OKS PREOP PAIN") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_PAIN) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position  
    ,
    
    # OKS PREOP NIGHT PAIN
    input_dataset%>%count(OKS_PREOP_NIGHT_PAIN) %>%
      mutate(VAR = "OKS PREOP NIGHT PAIN") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_NIGHT_PAIN) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position  
    ,
    
    # OKS PREOP WASHING
    input_dataset%>%count(OKS_PREOP_WASHING) %>%
      mutate(VAR = "OKS PREOP WASHING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_WASHING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position    
    ,
    
    # OKS PREOP TRANSPORT
    input_dataset%>%count(OKS_PREOP_TRANSPORT) %>%
      mutate(VAR = "OKS PREOP TRANSPORT") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_TRANSPORT) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position    
    ,
    
    # OKS PREOP WALKING
    input_dataset%>%count(OKS_PREOP_WALKING) %>%
      mutate(VAR = "OKS PREOP WALKING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_WALKING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position     
    ,
    
    # OKS PREOP STANDING
    input_dataset%>%count(OKS_PREOP_STANDING) %>%
      mutate(VAR = "OKS PREOP STANDING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_STANDING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position    
    ,
    
    # OKS PREOP LIMPING
    input_dataset%>%count(OKS_PREOP_LIMPING) %>%
      mutate(VAR = "OKS PREOP LIMPING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_LIMPING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position   
    ,
    
    # OKS PREOP KNEELING
    input_dataset%>%count(OKS_PREOP_KNEELING) %>%
      mutate(VAR = "OKS PREOP KNEELING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_KNEELING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position   
    ,
    
    # OKS PREOP WORK
    input_dataset%>%count(OKS_PREOP_WORK) %>%
      mutate(VAR = "OKS PREOP WORK") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_WORK) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position    
    ,
    
    # OKS PREOP CONFIDENCE
    input_dataset%>%count(OKS_PREOP_CONFIDENCE) %>%
      mutate(VAR = "OKS PREOP CONFIDENCE") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_CONFIDENCE) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position   
    ,
    
    # OKS PREOP SHOPPING
    input_dataset%>%count(OKS_PREOP_SHOPPING) %>%
      mutate(VAR = "OKS PREOP SHOPPING") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_SHOPPING) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position     
    ,
    
    # OKS PREOP STAIRS
    input_dataset%>%count(OKS_PREOP_STAIRS) %>%
      mutate(VAR = "OKS PREOP STAIRS") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_PREOP_STAIRS) %>% 
      select(c(VAR,VALUE_COL), everything()) #to change columns' position     
    ,
    
    # OKS MCID
    input_dataset%>%count(OKS_MCID) %>%
      mutate(VAR = "OKS MCID") %>% 
      mutate(PERCENT = round(100*(n / sum(n)),2)) %>%
      dplyr::rename(VALUE_COL = OKS_MCID) %>% 
      mutate(VALUE_COL=ifelse(VALUE_COL==1, "YES","NO")) %>%  # OKS_MCID = 1 -> YES , OKS_MCID = 0 -> NO
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

# DescriptiveCont2.OKS fun ----------------------------------------------------
#### DESCRIPTIVE FOR KNEES: CONTINUOUS
DescriptiveCont2.OKS<-function(input_dataset, output_table){
  DescriptiveContTable<- rbind(
    
    ##OKS PRE-OP
    inner_join(
      #OKS preop median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(OKS_PREOP_TOTSCORE,0.5),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="OKS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #OKS preop Q1
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(OKS_PREOP_TOTSCORE,0.25),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="OKS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #OKS preop Q3
      input_dataset%>%
        summarise(
          Q3 = round(quantile(OKS_PREOP_TOTSCORE,0.75),2) )%>%
        mutate(VALUE_COL="PreOperative")%>%
        mutate(VARIABLE="OKS Total Score")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    ,
    ##OKS POST-OP
    inner_join(
      #OKS postop median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(OKS_POSTOP_TOTSCORE,0.5),2) )%>%
                   mutate(VALUE_COL="PostOperative")%>%
                   mutate(VARIABLE="OKS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #OKS postop Q1
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(OKS_POSTOP_TOTSCORE, 0.25),2) )%>%
                   mutate(VALUE_COL="PostOperative")%>%
                   mutate(VARIABLE="OKS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #OKS postop Q3
      input_dataset%>%
        summarise(
          Q3 = round(quantile(OKS_POSTOP_TOTSCORE, 0.75),2) )%>%
        mutate(VALUE_COL="PostOperative")%>%
        mutate(VARIABLE="OKS Total Score")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    ,
    
    ##OKS_TOTSCORE.diff
    inner_join(
      #OKS_TOTSCORE.diff median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(OKS_TOTSCORE.diff, 0.5),2) )%>%
                   mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
                   mutate(VARIABLE="OKS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #OKS_TOTSCORE.diff Q1
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(OKS_TOTSCORE.diff,0.25),2) )%>%
                   mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
                   mutate(VARIABLE="OKS Total Score")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #OKS_TOTSCORE.diff Q3
      input_dataset%>%
        summarise(
          Q3 = round(quantile(OKS_TOTSCORE.diff,0.75),2) )%>%
        mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
        mutate(VARIABLE="OKS Total Score")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    
    ,
    
    ##EQ-VAS PRE-OP
    inner_join(
      #VAS preop Median
      inner_join(input_dataset%>%
                   summarise(
                     Median = round(quantile(EQ5D_PREOP_VAS, 0.5, na.rm = T),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 #VAS preop Q1 
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(EQ5D_PREOP_VAS, 0.25, na.rm = T),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      #VAS preop Q3 
      input_dataset%>%
        summarise(
          Q3 = round(quantile(EQ5D_PREOP_VAS, 0.75, na.rm = T),2) )%>%
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
                     Median = round(quantile(EQ5D_POSTOP_VAS, 0.5, na.rm = T),2) )%>%
                   mutate(VALUE_COL="PostOperative")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 #VAS postop Q1
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(EQ5D_POSTOP_VAS, 0.25, na.rm = T),2) )%>%
                   mutate(VALUE_COL="PostOperative")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #VAS postop Q3
      input_dataset%>%
        summarise(
          Q3 = round(quantile(EQ5D_POSTOP_VAS, 0.75, na.rm = T),2) )%>%
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
                     Median = round(quantile(VAS_TOTSCORE.diff,0.5, na.rm = T),2) )%>%
                   mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #VAS_TOTSCORE.diff Q1 
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(VAS_TOTSCORE.diff, 0.25, na.rm = T),2) )%>%
                   mutate(VALUE_COL="difference (PostOp-PreOp)")%>%
                   mutate(VARIABLE="EQ-VAS")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #VAS_TOTSCORE.diff Q3 
      input_dataset%>%
        summarise(
          Q3 = round(quantile(VAS_TOTSCORE.diff, 0.75, na.rm = T),2) )%>%
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
                     Median = round(quantile(EQ5D_PREOP_INDEX, 0.5, na.rm = T),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="EQ5D INDEX")%>% 
                   select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
                 ,
                 
                 #EQ5D PRE-OP INDEX Q1 
                 input_dataset%>%
                   summarise(
                     Q1 = round(quantile(EQ5D_PREOP_INDEX, 0.25, na.rm = T),2) )%>%
                   mutate(VALUE_COL="PreOperative")%>%
                   mutate(VARIABLE="EQ5D INDEX")%>% 
                   select(c(VARIABLE,VALUE_COL), everything())) #to change columns' position
      ,
      
      #EQ5D PRE-OP INDEX Q3 
      input_dataset%>%
        summarise(
          Q3 = round(quantile(EQ5D_PREOP_INDEX, 0.75, na.rm = T),2) )%>%
        mutate(VALUE_COL="PreOperative")%>%
        mutate(VARIABLE="EQ5D INDEX")%>% 
        select(c(VARIABLE,VALUE_COL), everything()) #to change columns' position
    )
    
  )
  
  # assign the output_df name given as a function argument (GlobalEnv so it is accessible outside of the function) 
  assign(deparse(substitute(output_table)), DescriptiveContTable, envir=.GlobalEnv)
}
#.........................................................................................
#.........................................................................................


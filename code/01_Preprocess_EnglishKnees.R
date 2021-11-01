# Import libraries --------------------------------------------------------
source("code/00_ProjectFunctions.R") # library of custom-made functionslibrary(dplyr) # for df manipulation
library(ggplot2) # for plotting
library(gridExtra) # for plots' formatting
library(scales) # this is to adjust the scales of plots
library(naniar) # for missing values visualisation
library(reshape) # For data wrangling}
library(reshape2) # For data wrangling
library(tidyverse)# For data wrangling/2
library(caret) # Machine learning library
#.........................................................................................
#.........................................................................................

# Import raw datasets -----------------------------------------------------
input_folder<-"S:/ClinEng_Users/CEDAR/Training & KSF info and forms/STP/STP - AURA/MSc Uni Manchester/Dissertation/English Data/"
## 2011-12
Eng_knee_11_12<-read.csv(paste0(input_folder,"2011-2012/Knee Replacement 1112.csv"), header = T)
## 2012-13
Eng_knee_12_13<-read.csv(paste0(input_folder,"2012-2013/Record Level Knee Replacement 1213.csv"), header = T)
## 2013-14
Eng_knee_13_14<-read.csv(paste0(input_folder,"2013-2014/Record Level Knee Replacement 1314.csv"), header = T)
## 2014-15
Eng_knee_14_15<-read.csv(paste0(input_folder,"2014-2015/Record Level Knee Replacement 1415.csv"), header = T)
## 2015-16
Eng_knee_15_16<-read.csv(paste0(input_folder,"2015-2016/Knee Replacement 1516.csv"), header = T)
## 2016-17
Eng_knee_16_17<-read.csv(paste0(input_folder,"2016-2017/Knee Replacement Provider 1617.csv"), header = T)
## 2017-18
Eng_knee_17_18<-read.csv(paste0(input_folder,"2017-2018/Knee Replacements Provider 1718.csv"), header = T)
## 2018-19
Eng_knee_18_19<-read.csv(paste0(input_folder,"2018-2019/Knee Replacement Provider 1819.csv"), header = T)
## 2019-20
Eng_knee_19_20<-read.csv(paste0(input_folder,"2019-2020/Knee Replacement Provider 1920.csv"), header = T)
#.........................................................................................
#.........................................................................................

# Change column names -----------------------------------------------------
new_col_names_knees<-c("PROVIDER",
                       "PROCEDURE",
                       "REVISION",
                       "YEAR",
                       "AGEBAND",
                       "SEX",
                       "PREOP_ASSISTED",
                       "PREOP_ASSISTED_BY",
                       "PREOP_SYMPTOM_PERIOD",
                       "PREOP_PREVIOUS_SURGERY",
                       "PREOP_LIVING_ARRANGEMENTS",
                       "PREOP_DISABILITY",
                       "HEART_DISEASE",
                       "HIGH_BP", 
                       "STROKE",
                       "CIRCULATION",
                       "LUNG_DISEASE",
                       "DIABETES",
                       "KIDNEY_DISEASE",
                       "NERVOUS_SYSTEM",
                       "LIVER_DISEASE",
                       "CANCER",
                       "DEPRESSION",
                       "ARTHRITIS",
                       "EQ5D_PREOP_MOBILITY",
                       "EQ5D_PREOP_SELFCARE",
                       "EQ5D_PREOP_ACTIVITY",
                       "EQ5D_PREOP_DISCOMFORT",
                       "EQ5D_PREOP_ANXIETY",
                       "EQ5D_PREOP_PROFILE",
                       "EQ5D_PREOP_INDEX",
                       "POSTOP_ASSISTED",
                       "POSTOP_ASSISTED_BY",
                       "POSTOP_LIVING_ARRANGEMENTS",
                       "POSTOP_DISABILITY",
                       "EQ5D_POSTOP_MOBILITY",
                       "EQ5D_POSTOP_SELFCARE",
                       "EQ5D_POSTOP_ACTIVITY",
                       "EQ5D_POSTOP_DISCOMFORT",
                       "EQ5D_POSTOP_ANXIETY",
                       "POSTOP_SATISFACTION",
                       "POSTOP_SUCCESS",
                       "POSTOP_ALLERGY",
                       "POSTOP_BLEEDING",
                       "POSTOP_WOUND",
                       "POSTOP_URINE",
                       "POSTOP_FURTHERSURGERY",
                       "POSTOP_READMITTED",
                       "EQ5D_POSTOP_PROFILE",
                       "EQ5D_POSTOP_INDEX",
                       "EQ5D_POSTOP_INDEX_NHSENG_PREDICTED",
                       "EQ5D_PREOP_VAS",
                       "EQ5D_POSTOP_VAS",
                       "EQ5D_POSTOP_VAS_NHSENG_PREDICTED",
                       "OKS_PREOP_PAIN",
                       "OKS_PREOP_NIGHT_PAIN",
                       "OKS_PREOP_WASHING",
                       "OKS_PREOP_TRANSPORT",
                       "OKS_PREOP_WALKING",
                       "OKS_PREOP_STANDING",
                       "OKS_PREOP_LIMPING",
                       "OKS_PREOP_KNEELING",
                       "OKS_PREOP_WORK",
                       "OKS_PREOP_CONFIDENCE",
                       "OKS_PREOP_SHOPPING",
                       "OKS_PREOP_STAIRS",
                       "OKS_PREOP_TOTSCORE",
                       "OKS_POSTOP_PAIN",
                       "OKS_POSTOP_NIGHT_PAIN",
                       "OKS_POSTOP_WASHING",
                       "OKS_POSTOP_TRANSPORT",
                       "OKS_POSTOP_WALKING",
                       "OKS_POSTOP_STANDING",
                       "OKS_POSTOP_LIMPING",
                       "OKS_POSTOP_KNEELING",
                       "OKS_POSTOP_WORK",
                       "OKS_POSTOP_CONFIDENCE",
                       "OKS_POSTOP_SHOPPING",
                       "OKS_POSTOP_STAIRS",
                       "OKS_POSTOP_TOTSCORE",
                       "OKS_POSTOP_TOTSCORE_NHSENG_PREDICTED")
## 2011-12 only
change_col_names(input_df = 'Eng_knee_11_12',
                 output_df = 'Eng_knee_11_12_NEW',
                 #dataset 2011-12 has reduced N of cols
                 new_col_names = new_col_names_knees[-c(3,7:24,32:35)] 
)
## 2012-13 only
change_col_names(input_df = 'Eng_knee_12_13',
                 output_df = 'Eng_knee_12_13_NEW',
                 #dataset 2012-13 has reduced N of cols
                 new_col_names = new_col_names_knees[-c(7:24,32:35)] 
)
## from 2013-14 to 2019-20
# create a vector with all relevant tables' names
knee_tables_13_20<-c("Eng_knee_13_14",
                     "Eng_knee_14_15",
                     "Eng_knee_15_16",
                     "Eng_knee_16_17",
                     "Eng_knee_17_18",
                     "Eng_knee_18_19",
                     "Eng_knee_19_20"
)
for (tb in knee_tables_13_20){
  change_col_names(input_df = tb,
                   output_df =  paste0(tb,'_NEW'),
                   new_col_names = new_col_names_knees)
}
#.........................................................................................
#.........................................................................................


# Merge datasets ----------------------------------------------------------
English.dataset.knee<-rbind( merge(merge(Eng_knee_11_12_NEW,Eng_knee_12_13_NEW, all = T),Eng_knee_13_14_NEW, all = T),
                             Eng_knee_14_15_NEW,
                             Eng_knee_15_16_NEW,
                             Eng_knee_16_17_NEW,
                             Eng_knee_17_18_NEW,
                             Eng_knee_18_19_NEW,
                             Eng_knee_19_20_NEW
)
# reorder English.dataset.knee df by column name
English.dataset.knee <- English.dataset.knee[new_col_names_knees]
#.........................................................................................
#.........................................................................................

# Dealing with NAs --------------------------------------------------------
#substituting * and 9 in the relevant columns with NAs
is.na(English.dataset.knee) <- English.dataset.knee == "*" # substituting '*' with NA
#substituting 9 in the relevant columns with NAs
is.na(English.dataset.knee[,c(5:12,25:29,32:48,55:66,68:79)]) <- English.dataset.knee[,c(5:12,25:29,32:48,55:66,68:79)] == 9 # substituting 9 with NA in selected columns
#substituting 999 in the relevant columns with NAs
is.na(English.dataset.knee) <- English.dataset.knee == 999 # substituting 999 with NA in selected columns
## count N patients for each year
#English.dataset.knee %>% group_by(YEAR) %>% count()
## count N missing values by column
#sapply(English.dataset.knee, function(x){sum(is.na(x))})
rm(list=ls(pattern="Eng_knee*"))
rm(knee_tables_13_20,input_folder,new_col_names_knees,tb)
### add ID column to the final table
English.dataset.knee$ID <- seq.int(nrow(English.dataset.knee))
## I calculate the N of missing values by subject for 
## either OKS pre or post-op, adding two extra cols at the end of the dataframe:
English.dataset.knee<-as.data.frame(English.dataset.knee %>% rowwise() %>% 
                                      mutate(missVal_OKS_PREOP = sum(is.na(c(OKS_PREOP_PAIN,
                                                                             OKS_PREOP_NIGHT_PAIN,
                                                                             OKS_PREOP_WASHING,
                                                                             OKS_PREOP_TRANSPORT,
                                                                             OKS_PREOP_WALKING,
                                                                             OKS_PREOP_STANDING,
                                                                             OKS_PREOP_LIMPING,
                                                                             OKS_PREOP_KNEELING,
                                                                             OKS_PREOP_WORK,
                                                                             OKS_PREOP_CONFIDENCE,
                                                                             OKS_PREOP_SHOPPING,
                                                                             OKS_PREOP_STAIRS))),
                                             
                                             missVal_OKS_POSTOP = sum(is.na(c(OKS_POSTOP_PAIN,
                                                                              OKS_POSTOP_NIGHT_PAIN,
                                                                              OKS_POSTOP_WASHING,
                                                                              OKS_POSTOP_TRANSPORT,
                                                                              OKS_POSTOP_WALKING,
                                                                              OKS_POSTOP_STANDING,
                                                                              OKS_POSTOP_LIMPING,
                                                                              OKS_POSTOP_KNEELING,
                                                                              OKS_POSTOP_WORK,
                                                                              OKS_POSTOP_CONFIDENCE,
                                                                              OKS_POSTOP_SHOPPING,
                                                                              OKS_POSTOP_STAIRS)))))
##I want to check distributions of subjects by number of missing values for pre and post-op OHS
#English.dataset.knee %>% count(missVal_OKS_PREOP) 
#imputation could be done for subjects with max of 2 missing values (there is none, so they have possibly been already imputed! :) )
#English.dataset.knee %>% count(missVal_OKS_POSTOP) 
## no imputation is required as no subject has 1 or 2 missing values for pre/post-op OHS
## (see Murray et al. 2007 rules)

#.........................................................................................
#.........................................................................................

# Keep only relevant years ------------------------------------------------
English.dataset.knee_extended<-English.dataset.knee[!(English.dataset.knee$YEAR == "2011/12" |
                                                        English.dataset.knee$YEAR == "2012/13"),]
#English.dataset.knee_extended %>% count(YEAR)
#.........................................................................................
#.........................................................................................

# Data cleaning -----------------------------------------------------------
##Identify variables with 0 or near-0 variance
NZV<-nearZeroVar(English.dataset.knee_extended, saveMetrics = T)
#NZV[NZV$zeroVar==TRUE | NZV$nzv==TRUE ,]
# Remove variables with zero variance:
English.dataset.knee_extended_cln<-English.dataset.knee_extended[,!names(English.dataset.knee_extended) %in% 
                                                                   c("PROCEDURE","PREOP_ASSISTED_BY")]
## Remove all post-op variables except those of interest (EQ-VAS and Q scores)
to_remove1<-c("POSTOP_ASSISTED",
              "POSTOP_ASSISTED_BY",
              "POSTOP_LIVING_ARRANGEMENTS",
              "POSTOP_DISABILITY",
              "EQ5D_POSTOP_MOBILITY",
              "EQ5D_POSTOP_SELFCARE",
              "EQ5D_POSTOP_ACTIVITY",
              "EQ5D_POSTOP_DISCOMFORT",
              "EQ5D_POSTOP_ANXIETY",
              "POSTOP_SATISFACTION",
              "POSTOP_SUCCESS",
              "POSTOP_ALLERGY",
              "POSTOP_BLEEDING",
              "POSTOP_WOUND",
              "POSTOP_URINE",
              "POSTOP_FURTHERSURGERY",
              "POSTOP_READMITTED",
              "EQ5D_POSTOP_PROFILE",
              "EQ5D_POSTOP_INDEX",
              "EQ5D_POSTOP_INDEX_NHSENG_PREDICTED",                     
              "OKS_POSTOP_PAIN",
              "OKS_POSTOP_NIGHT_PAIN",
              "OKS_POSTOP_WASHING",
              "OKS_POSTOP_TRANSPORT",
              "OKS_POSTOP_WALKING",
              "OKS_POSTOP_STANDING",
              "OKS_POSTOP_LIMPING",
              "OKS_POSTOP_KNEELING",
              "OKS_POSTOP_WORK",
              "OKS_POSTOP_CONFIDENCE",
              "OKS_POSTOP_SHOPPING",
              "OKS_POSTOP_STAIRS")
English.dataset.knee_extended_cln2<-English.dataset.knee_extended_cln[,!names(English.dataset.knee_extended_cln) 
                                                                      %in% to_remove1]
# Removing other variables not important for the analysis
# PROVIDER, PREOP_SYMPTOM_PERIOD, EQ5D_PREOP_PROFILE, EQ5D_POSTOP_VAS_NHSENG_PREDICTED, 
# OKS_POSTOP_TOTSCORE_NHSENG_PREDICTED
to_remove2<-c("PROVIDER",
              "PREOP_SYMPTOM_PERIOD",
              "EQ5D_PREOP_PROFILE",
              "EQ5D_POSTOP_VAS_NHSENG_PREDICTED",
              "OKS_POSTOP_TOTSCORE_NHSENG_PREDICTED")

English.dataset.knee_extended_cln3<-English.dataset.knee_extended_cln2[,-c(1,7,28,32,47)]
## Remove all rows with missing values*
English.dataset.knee_extended_cln4<-English.dataset.knee_extended_cln3[complete.cases(English.dataset.knee_extended_cln3),]
#English.dataset.knee_extended_cln4 %>% count(YEAR)
#names(English.dataset.knee_extended_cln4)
names(English.dataset.hip_extended_cln4)
#.........................................................................................
#.........................................................................................

# Create outcome variable MCID --------------------------------------------
## calculating std deviation for preop EQ-VAS variable so that the EQ5-VAS MCID threshold
## can be identified
English.dataset.knee_extended_cln4 %>% summarise(Mean = mean(EQ5D_PREOP_VAS),
                                                 stdDev = sd(EQ5D_PREOP_VAS))
##Create OKS & EQ-VAS MCID for HIPS
English.dataset.knee_extended_cln4 <- as.data.frame(
  English.dataset.knee_extended_cln4 %>% 
    mutate(OKS_TOTSCORE.diff = OKS_POSTOP_TOTSCORE - OKS_PREOP_TOTSCORE) %>%
    mutate(VAS_TOTSCORE.diff = EQ5D_POSTOP_VAS - EQ5D_PREOP_VAS) %>%
    # threshold value from literature 
    # (postOKS - preOKS >= 7 points of increase)
    mutate(OKS_MCID = ifelse(OKS_TOTSCORE.diff>=7, 1,0))%>%
    # threshold value from pre-op EQ-VAS distributions
    # (postvas - preVAS >= 10 points of increase ---> 11 is 0.5*std_dev(preVAS))
    # std_dev(preVAS) --> this value has been calculated at step 13.2
    mutate(VAS_MCID = ifelse(VAS_TOTSCORE.diff>=10, 1,0)))
#.........................................................................................
#.........................................................................................

# Extracting English Training set (Year = 2016/17+2017/18) + Demog --------
English.training.1618<-English.dataset.knee_extended_cln4[
  English.dataset.knee_extended_cln4$YEAR=='2016/17'|
    English.dataset.knee_extended_cln4$YEAR=='2017/18' ,]
#table(English.training.1618$YEAR)
### for categorical variables
DescriptiveCatBasic.OKS(input_dataset = English.training.1618, 
                        output_table = Descr1.English.training.1618)
DescriptiveCatExtra(input_dataset = English.training.1618, 
                    output_table = Descr2.English.training.1618)
DescriptiveCont2.OKS(input_dataset = English.training.1618, 
                     output_table = Descr3.English.training.1618)
write.csv(Descr1.English.training.1618,"output/thesis_files/Descr1.English.training.1618_knees.csv", row.names = FALSE)
write.csv(Descr2.English.training.1618,"output/thesis_files/Descr2.English.training.1618_knees.csv", row.names = FALSE)
write.csv(Descr3.English.training.1618,"output/thesis_files/Descr3.English.training.1618_knees.csv", row.names = FALSE)

















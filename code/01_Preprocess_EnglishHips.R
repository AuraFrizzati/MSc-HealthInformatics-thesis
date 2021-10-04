### [0] import relevant libraries

source("S:/ClinEng_Users/CEDAR/Training & KSF info and forms/STP/STP - AURA/MSc Uni Manchester/Dissertation/code/R/ProjectFUN.R") # custom-made functions
library(dplyr) # for df manipulation
library(ggplot2) # for plotting
library(gridExtra) # for plots' formatting
library(scales) # this is to adjust the scales of plots
library(naniar) # for missing values visualisation
library(reshape) # For data wrangling}
library(reshape2) # For data wrangling
library(tidyverse)# For data wrangling/2
library(caret) # Machine learning library

#**************
### [1] import raw datasets
input_folder<-"S:/ClinEng_Users/CEDAR/Training & KSF info and forms/STP/STP - AURA/MSc Uni Manchester/Dissertation/English Data/"
## 2011-12
Eng_hip_11_12<-read.csv(paste0(input_folder,"2011-2012/Hip Replacement 1112.csv"), header = T)
## 2012-13
Eng_hip_12_13<-read.csv(paste0(input_folder,"2012-2013/Record Level Hip Replacement 1213.csv"), header = T)
## 2013-14
Eng_hip_13_14<-read.csv(paste0(input_folder,"2013-2014/Record Level Hip Replacement 1314.csv"), header = T)
## 2014-15
Eng_hip_14_15<-read.csv(paste0(input_folder,"2014-2015/Record Level Hip Replacement 1415.csv"), header = T)
## 2015-16
Eng_hip_15_16<-read.csv(paste0(input_folder,"2015-2016/Hip Replacement 1516.csv"), header = T)
## 2016-17
Eng_hip_16_17<-read.csv(paste0(input_folder,"2016-2017/Hip Replacement Provider 1617.csv"), header = T)
## 2017-18
Eng_hip_17_18<-read.csv(paste0(input_folder,"2017-2018/Hip Replacements Provider 1718.csv"), header = T)
## 2018-19
Eng_hip_18_19<-read.csv(paste0(input_folder,"2018-2019/Hip Replacement Provider 1819.csv"), header = T)
## 2019-20
Eng_hip_19_20<-read.csv(paste0(input_folder,"2019-2020/Hip Replacement Provider 1920.csv"), header = T)

#**************
## [2] Change column names
new_col_names_hips<-c("PROVIDER",
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
                      "OHS_PREOP_PAIN",
                      "OHS_PREOP_SUDDENPAIN",
                      "OHS_PREOP_NIGHTPAIN",
                      "OHS_PREOP_WASHING",
                      "OHS_PREOP_TRANSPORT",
                      "OHS_PREOP_DRESSING",
                      "OHS_PREOP_SHOPPING",
                      "OHS_PREOP_WALKING",
                      "OHS_PREOP_LIMPING",
                      "OHS_PREOP_STAIRS",
                      "OHS_PREOP_STANDING",
                      "OHS_PREOP_WORK",
                      "OHS_PREOP_TOTSCORE",
                      "OHS_POSTOP_PAIN",
                      "OHS_POSTOP_SUDDENPAIN",
                      "OHS_POSTOP_NIGHTPAIN",
                      "OHS_POSTOP_WASHING",
                      "OHS_POSTOP_TRANSPORT",
                      "OHS_POSTOP_DRESSING",
                      "OHS_POSTOP_SHOPPING",
                      "OHS_POSTOP_WALKING",
                      "OHS_POSTOP_LIMPING",
                      "OHS_POSTOP_STAIRS",
                      "OHS_POSTOP_STANDING",
                      "OHS_POSTOP_WORK",
                      "OHS_POSTOP_TOTSCORE",
                      "OHS_POSTOP_TOTSCORE_NHSENG_PREDICTED")


## 2011-12 only

change_col_names(input_df = 'Eng_hip_11_12',
                 output_df = 'Eng_hip_11_12_NEW',
                 #dataset 2011-12 has reduced N of cols
                 new_col_names = new_col_names_hips[-c(3,7:24,32:35)] 
)

## 2012-13 only

change_col_names(input_df = 'Eng_hip_12_13',
                 output_df = 'Eng_hip_12_13_NEW',
                 #dataset 2012-13 has reduced N of cols
                 new_col_names = new_col_names_hips[-c(7:24,32:35)] 
)

## from 2013-14 to 2019-20
# create a vector with all relevant tables' names
hip_tables_13_20<-c("Eng_hip_13_14",
                    "Eng_hip_14_15",
                    "Eng_hip_15_16",
                    "Eng_hip_16_17",
                    "Eng_hip_17_18",
                    "Eng_hip_18_19",
                    "Eng_hip_19_20"
)

for (tb in hip_tables_13_20){
  change_col_names(input_df = tb,
                   output_df =  paste0(tb,'_NEW'),
                   new_col_names = new_col_names_hips)
}

#**************
## [3] Merge datasets

English.dataset.hip<-rbind( merge(merge(Eng_hip_11_12_NEW,Eng_hip_12_13_NEW, all = T),Eng_hip_13_14_NEW, all = T),
                            Eng_hip_14_15_NEW,
                            Eng_hip_15_16_NEW,
                            Eng_hip_16_17_NEW,
                            Eng_hip_17_18_NEW,
                            Eng_hip_18_19_NEW,
                            Eng_hip_19_20_NEW
)

# reorder English.dataset.hip df by column name
English.dataset.hip <- English.dataset.hip[new_col_names_hips]

#**************
### [4] Dealing with NAs

#substituting *
is.na(English.dataset.hip) <- English.dataset.hip == "*" # substituting '*' with NA

#substituting 9 in the relevant columns with NAs
is.na(English.dataset.hip[,c(5:12,25:29,32:48,55:66,68:79)]) <- English.dataset.hip[,c(5:12,25:29,32:48,55:66,68:79)] == 9 # substituting 9 with NA in selected columns

#substituting 999 in the relevant columns with NAs
is.na(English.dataset.hip) <- English.dataset.hip == 999 # substituting 999 with NA in selected columns

####

## count N patients for each year
English.dataset.hip %>% group_by(YEAR) %>% count()

## count N missing values by column
sapply(English.dataset.hip, function(x){sum(is.na(x))})

####

rm(list=ls(pattern="Eng_hip*"))
rm(hip_tables_13_20,input_folder,new_col_names_hips,tb)

####

### add ID column to the final table
English.dataset.hip$ID <- seq.int(nrow(English.dataset.hip))

## I calculate the N of missing values by subject for 
## either OHS pre or post-op, adding two extra cols at the end of the dataframe:
English.dataset.hip<-as.data.frame(English.dataset.hip %>% 
                                     rowwise() %>% 
                                     mutate(missVal_OHS_PREOP = 
                                              sum(is.na(c(
                                                OHS_PREOP_PAIN,
                                                OHS_PREOP_SUDDENPAIN,
                                                OHS_PREOP_NIGHTPAIN,
                                                OHS_PREOP_WASHING,
                                                OHS_PREOP_TRANSPORT,
                                                OHS_PREOP_DRESSING,
                                                OHS_PREOP_SHOPPING,
                                                OHS_PREOP_WALKING,
                                                OHS_PREOP_LIMPING,
                                                OHS_PREOP_STAIRS,
                                                OHS_PREOP_STANDING,
                                                OHS_PREOP_WORK))),
                                            missVal_OHS_POSTOP = 
                                              sum(is.na(c(
                                                OHS_POSTOP_PAIN,
                                                OHS_POSTOP_SUDDENPAIN,
                                                OHS_POSTOP_NIGHTPAIN,
                                                OHS_POSTOP_WASHING,
                                                OHS_POSTOP_TRANSPORT,
                                                OHS_POSTOP_DRESSING,
                                                OHS_POSTOP_SHOPPING,
                                                OHS_POSTOP_WALKING,
                                                OHS_POSTOP_LIMPING,
                                                OHS_POSTOP_STAIRS,
                                                OHS_POSTOP_STANDING,
                                                OHS_POSTOP_WORK)))))

#####

##Check distributions of subjects by number of missing values for 
## pre and post-op OHS
English.dataset.hip %>% count(missVal_OHS_PREOP) 
# missVal_OHS_PREOP      n
# 1                  0 356584
# 2                  3    529
# 3                  4    198
# 4                  5    349
# 5                  6     63
# 6                  7     44
# 7                  8     58
# 8                  9     21
# 9                 10   2509
# 10                11     36
# 11                12     97
English.dataset.hip %>% count(missVal_OHS_POSTOP)
# missVal_OHS_POSTOP      n
# 1                   0 355889
# 2                   3   1365
# 3                   4    560
# 4                   5    483
# 5                   6    214
# 6                   7    162
# 7                   8    104
# 8                   9    126
# 9                  10   1130
# 10                 11     90
# 11                 12    365

## no imputation is required as no subject has 1 or 2 missing values for pre/post-op OHS
## (see Murray et al. 2007 rules)

#**************
## [5] Keeping only relevant years --> only from 2012/13 onwards
English.dataset.hip_extended<-English.dataset.hip[!(English.dataset.hip$YEAR == "2011/12" |
                                                      English.dataset.hip$YEAR == "2012/13"),]


English.dataset.hip_extended %>% count(YEAR)
#   YEAR     n
# 2013/14 44650
# 2014/15 45554
# 2015/16 43274
# 2016/17 44241
# 2017/18 39323
# 2018/19 41280
# 2019/20 21880

#**************
## [6] Data cleaning

##Identify variables with 0 or near-0 variance
NZV<-nearZeroVar(English.dataset.hip_extended, saveMetrics = T)
NZV[NZV$zeroVar==TRUE | NZV$nzv==TRUE ,]
#                       freqRatio percentUnique zeroVar  nzv
# PROCEDURE               0.00000  0.0003568854    TRUE TRUE
# PREOP_ASSISTED_BY       0.00000  0.0003568854    TRUE TRUE
# STROKE                 70.60797  0.0007137708   FALSE TRUE
# CIRCULATION            19.17003  0.0007137708   FALSE TRUE
# KIDNEY_DISEASE         49.60538  0.0007137708   FALSE TRUE
# NERVOUS_SYSTEM        120.72111  0.0007137708   FALSE TRUE
# LIVER_DISEASE         161.53016  0.0007137708   FALSE TRUE
# POSTOP_BLEEDING        23.67919  0.0007137708   FALSE TRUE
# POSTOP_FURTHERSURGERY  43.07995  0.0007137708   FALSE TRUE
# missVal_OHS_PREOP     132.41826  0.0039257393   FALSE TRUE
# missVal_OHS_POSTOP    286.62526  0.0039257393   FALSE TRUE


# Remove variables with zero variance:
English.dataset.hip_extended_cln<-English.dataset.hip_extended[,!names(English.dataset.hip_extended) %in% 
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
              "OHS_POSTOP_PAIN",
              "OHS_POSTOP_SUDDENPAIN",
              "OHS_POSTOP_NIGHTPAIN",
              "OHS_POSTOP_WASHING",
              "OHS_POSTOP_TRANSPORT",
              "OHS_POSTOP_DRESSING",
              "OHS_POSTOP_SHOPPING",
              "OHS_POSTOP_WALKING",
              "OHS_POSTOP_LIMPING",
              "OHS_POSTOP_STAIRS",
              "OHS_POSTOP_STANDING",
              "OHS_POSTOP_WORK")

English.dataset.hip_extended_cln2<-English.dataset.hip_extended_cln[,!names(English.dataset.hip_extended_cln) 
                                                                    %in% to_remove1]

# Removing other variables not important for the analysis
# PROVIDER, PREOP_SYMPTOM_PERIOD, EQ5D_PREOP_PROFILE, EQ5D_POSTOP_VAS_NHSENG_PREDICTED, 
# OHS_POSTOP_TOTSCORE_NHSENG_PREDICTED
to_remove2<-c("PROVIDER",
              "PREOP_SYMPTOM_PERIOD",
              "EQ5D_PREOP_PROFILE",
              "EQ5D_POSTOP_VAS_NHSENG_PREDICTED",
              "OHS_POSTOP_TOTSCORE_NHSENG_PREDICTED")

English.dataset.hip_extended_cln3<-English.dataset.hip_extended_cln2[,-c(1,7,28,32,47)]
#names(English.dataset.hip_extended_cln3)

## Remove all rows with missing values
English.dataset.hip_extended_cln4<-English.dataset.hip_extended_cln3[complete.cases(English.dataset.hip_extended_cln3),]
English.dataset.hip_extended_cln4 %>% count(YEAR)

#     YEAR     n
# 1 2013/14 32448
# 2 2014/15 32416
# 3 2015/16 30724
# 4 2016/17 31598
# 5 2017/18 28042
# 6 2018/19 29659
# 7 2019/20 14775

names(English.dataset.hip_extended_cln4)

#**************
#### [7] Creating outcome variables MCID

## calculating std deviation for preop EQ-VAS variable so that the EQ5-VAS MCID threshold
## can be identified
English.dataset.hip_extended_cln4 %>% summarise(Mean = mean(EQ5D_PREOP_VAS),
                                                stdDev = sd(EQ5D_PREOP_VAS))

#    Mean   stdDev
# 64.49788 21.99992 --> about 22/2 = 11 as MCID threshold for VAS

##Create OHS & EQ-VAS MCID
English.dataset.hip_extended_cln4 <- as.data.frame(
  English.dataset.hip_extended_cln4 %>% 
    mutate(OHS_TOTSCORE.diff = OHS_POSTOP_TOTSCORE - OHS_PREOP_TOTSCORE) %>%
    mutate(VAS_TOTSCORE.diff = EQ5D_POSTOP_VAS - EQ5D_PREOP_VAS) %>%
    # threshold value from literature 
    # (postOHS - preOHS >= 8 points of increase)
    mutate(OHS_MCID = ifelse(OHS_TOTSCORE.diff>=8, 1,0))%>%
    # threshold value from pre-op EQ-VAS distributions
    # (postvas - preVAS >= 11 points of increase ---> 11 is 0.5*std_dev(preVAS))
    # std_dev(preVAS) --> this value has been calculated at step 13.2
    mutate(VAS_MCID = ifelse(VAS_TOTSCORE.diff>=11, 1,0)))


#**************
#### [8] Creating outcome variables OHS severity scores

English.dataset.hip_extended_cln4$OHS_POSTOP_CLASS<- NA
English.dataset.hip_extended_cln4[English.dataset.hip_extended_cln4$OHS_POSTOP_TOTSCORE <=19,]$OHS_POSTOP_CLASS<-1
English.dataset.hip_extended_cln4[English.dataset.hip_extended_cln4$OHS_POSTOP_TOTSCORE >19 &
                                    English.dataset.hip_extended_cln4$OHS_POSTOP_TOTSCORE <=29,]$OHS_POSTOP_CLASS<-2
English.dataset.hip_extended_cln4[English.dataset.hip_extended_cln4$OHS_POSTOP_TOTSCORE >29 &
                                    English.dataset.hip_extended_cln4$OHS_POSTOP_TOTSCORE <=39,]$OHS_POSTOP_CLASS<-3
English.dataset.hip_extended_cln4[English.dataset.hip_extended_cln4$OHS_POSTOP_TOTSCORE >39 &
                                    English.dataset.hip_extended_cln4$OHS_POSTOP_TOTSCORE <=48,]$OHS_POSTOP_CLASS<-4

English.dataset.hip_extended_cln4 %>% group_by(OHS_POSTOP_CLASS) %>% summarise (Min = min(OHS_POSTOP_TOTSCORE),
                                                                                Max = max(OHS_POSTOP_TOTSCORE))

# OHS_POSTOP_CLASS   Min   Max
#               1     0    19
#               2    20    29
#               3    30    39
#               4    40    48
# --> looks alright

#**************
#### [9] Demographics in the whole dataset

### for categorical variables
DescriptiveCatBasic(input_dataset = English.dataset.hip_extended_cln4, 
                    output_table = Descr1.English.dataset.hip_extended_cln4)

DescriptiveCatExtra(input_dataset = English.dataset.hip_extended_cln4, 
                    output_table = Descr2.English.dataset.hip_extended_cln4)

### for continuous variables
DescriptiveCont(input_dataset = English.dataset.hip_extended_cln4, 
                output_table = Descr3.English.dataset.hip_extended_cln4)

Descr3.English.dataset.hip_extended_cln4
#          VARIABLE                 VALUE_COL  Mean Std.Dev       toPrint
# 1 OHS Total Score              PreOperative 18.09    8.30   18.09 ± 8.3
# 2 OHS Total Score             PostOperative 39.66    8.69  39.66 ± 8.69
# 3 OHS Total Score difference (PostOp-PreOp) 21.57   10.14 21.57 ± 10.14
# 4          EQ-VAS              PreOperative 64.50   22.00     64.5 ± 22
# 5          EQ-VAS             PostOperative 77.17   17.83 77.17 ± 17.83
# 6          EQ-VAS difference (PostOp-PreOp) 12.67   23.46 12.67 ± 23.46
# 7      EQ5D INDEX              PreOperative  0.36    0.32   0.36 ± 0.32

#**************
#### [15] Extracting English Training set (Year = 2016/17+2017/18) + Demographics

English.training.1618<-English.dataset.hip_extended_cln4[
  English.dataset.hip_extended_cln4$YEAR=='2016/17'|
    English.dataset.hip_extended_cln4$YEAR=='2017/18' ,]

table(English.training.1618$YEAR)
# 2016/17 2017/18 
# 31598   28042 

### for categorical variables
DescriptiveCatBasic(input_dataset = English.training.1618, 
                    output_table = Descr1.English.training.1618)

DescriptiveCatExtra(input_dataset = English.training.1618, 
                    output_table = Descr2.English.training.1618)

### for continous variables
DescriptiveCont(input_dataset = English.training.1618, 
                output_table = Descr3.English.training.1618)


Descr3.English.training.1618
#          VARIABLE                 VALUE_COL  Mean Std.Dev       toPrint
# 1 OHS Total Score              PreOperative 17.87    8.26  17.87 ± 8.26
# 2 OHS Total Score             PostOperative 39.73    8.70   39.73 ± 8.7
# 3 OHS Total Score difference (PostOp-PreOp) 21.86   10.15 21.86 ± 10.15
# 4          EQ-VAS              PreOperative 63.76   22.53 63.76 ± 22.53
# 5          EQ-VAS             PostOperative 77.31   18.01 77.31 ± 18.01
# 6          EQ-VAS difference (PostOp-PreOp) 13.55   24.23 13.55 ± 24.23
# 7      EQ5D INDEX              PreOperative  0.35    0.32   0.35 ± 0.32

DescriptiveCont2(input_dataset = English.training.1618, 
                 output_table = NEW_Descr3.English.training.1618)

#         VARIABLE                 VALUE_COL Median    Q1    Q3
# 1 OHS Total Score              PreOperative  17.00 12.00 23.00
# 2 OHS Total Score             PostOperative  43.00 36.00 46.00
# 3 OHS Total Score difference (PostOp-PreOp)  23.00 16.00 29.00
# 4          EQ-VAS              PreOperative  70.00 50.00 80.00
# 5          EQ-VAS             PostOperative  80.00 70.00 90.00
# 6          EQ-VAS difference (PostOp-PreOp)  10.00  0.00 28.00
# 7      EQ5D INDEX              PreOperative   0.52  0.06  0.66

write.csv(Descr1.English.training.1618,"Descr1.English.training.1618.csv", row.names = FALSE)
write.csv(Descr2.English.training.1618,"Descr2.English.training.1618.csv", row.names = FALSE)
write.csv(Descr3.English.training.1618,"Descr3.English.training.1618.csv", row.names = FALSE)

write.csv(NEW_Descr3.English.training.1618,"NEW_Descr3.English.training.1618.csv", row.names = FALSE)





#**************
#### [16] Plotting distributions for relevant continuous in OHS and EQ-VAS
#### including MCID thresholding for English.training.1618 dataset

## Histogram of difference [OHS PostOp - PreOp]
Hist_OHS_training.1618<-ggplot(English.training.1618, aes(x = OHS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1,
                 #bins=20,
                 fill = "grey", colour="black")+
  ggtitle("PostOperative OHS change\n(English Training Set 2016/17+2017/18)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("Difference OHS PostOp - PreOp")+
  scale_x_continuous(limits=c(-48,48),breaks = seq(-48,48,4))+
  geom_vline(xintercept=8, colour="red", linetype = "longdash", size = 1)  +
  annotate(x=0,y=+Inf,label="MCID\nthreshold",vjust=2,geom="text", colour="red", size = 4)

## Histogram of difference [EQ-VAS PostOp - PreOp]
Hist_VAS_training.1618<-ggplot(English.training.1618, aes(x = VAS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1,
                 bins=20, 
                 fill = "grey", colour="black")+
  ggtitle("PostOperative EQ-VAS change\n(English Training Set 2016/17+2017/18)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("Difference EQ-VAS PostOp - PreOp")+
  scale_x_continuous(limits=c(-100,100),breaks = seq(-100,100,10))+
  geom_vline(xintercept=11, colour="red", linetype = "longdash", size = 1)  + #check the threshold line is plotted at std dev*0.5
  annotate(x=30,y=+Inf,label="MCID\nthreshold",vjust=2,geom="text", colour="red", size = 4)



## Histogram of preOp OHS
Hist_preopOHS_training.1618<-ggplot(English.training.1618, aes(x = OHS_PREOP_TOTSCORE)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PreOperative OHS tot score\n(English Training Set 2016/17+2017/18)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("PreOperative OHS total score ")+
  scale_x_continuous(limits=c(0,48),breaks = seq(0,48,4))

## Histogram of preOp EQ-VAS
Hist_preopVAS_training.1618<-ggplot(English.training.1618, aes(x = EQ5D_PREOP_VAS)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PreOperative EQ-VAS score\n(English Training Set 2016/17+2017/18)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("PreOperative EQ-VAS score")+
  scale_x_continuous(limits=c(0,100),breaks = seq(0,100,10))







#**************
#### [11] Extracting English Test set (Year = 2018/19) + Demographics

English.test.1819<-English.dataset.hip_extended_cln4[
  English.dataset.hip_extended_cln4$YEAR=='2018/19',]

### for categorical variables
DescriptiveCatBasic(input_dataset = English.test.1819, 
                    output_table = Descr1.English.test.1819)

DescriptiveCatExtra(input_dataset = English.test.1819, 
                    output_table = Descr2.English.test.1819)

### for continous variables
DescriptiveCont(input_dataset = English.test.1819, 
                output_table = Descr3.English.test.1819)


Descr3.English.test.1819
#           VARIABLE                 VALUE_COL  Mean Std.Dev       toPrint
# 1 OHS Total Score              PreOperative 17.58    8.22  17.58 ± 8.22
# 2 OHS Total Score             PostOperative 40.03    8.45  40.03 ± 8.45
# 3 OHS Total Score difference (PostOp-PreOp) 22.45   10.07 22.45 ± 10.07
# 4          EQ-VAS              PreOperative 63.31   22.32 63.31 ± 22.32
# 5          EQ-VAS             PostOperative 77.57   17.49 77.57 ± 17.49
# 6          EQ-VAS difference (PostOp-PreOp) 14.26   23.66 14.26 ± 23.66
# 7      EQ5D INDEX              PreOperative  0.34    0.32   0.34 ± 0.32

DescriptiveCont2(input_dataset = English.test.1819, 
                 output_table = NEW_Descr3.English.test.1819)

#          VARIABLE                 VALUE_COL Median    Q1    Q3
# 1 OHS Total Score              PreOperative  17.00 11.00 23.00
# 2 OHS Total Score             PostOperative  43.00 36.00 47.00
# 3 OHS Total Score difference (PostOp-PreOp)  23.00 16.00 30.00
# 4          EQ-VAS              PreOperative  70.00 50.00 80.00
# 5          EQ-VAS             PostOperative  80.00 70.00 90.00
# 6          EQ-VAS difference (PostOp-PreOp)  10.00  0.00 30.00
# 7      EQ5D INDEX              PreOperative   0.52  0.06  0.62


write.csv(Descr1.English.test.1819,"Descr1.English.test.1819.csv", row.names = FALSE)
write.csv(Descr2.English.test.1819,"Descr2.English.test.1819.csv", row.names = FALSE)
write.csv(Descr3.English.test.1819,"Descr3.English.test.1819.csv", row.names = FALSE)

write.csv(NEW_Descr3.English.test.1819,"NEW_Descr3.English.test.1819.csv", row.names = FALSE) 
#.........................................................................................
#.........................................................................................


# histograms for hips (pre and difference post-pre) -----------------------
#### Plotting distributions for relevant continuous vars in OHS and EQ-VAS
#### including MCID thresholding for English.training.1618 dataset

## Histogram of difference [OHS PostOp - PreOp]
Hist_OHS_English.test.1819<-ggplot(English.test.1819, aes(x = OHS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1,
                 #bins=20,
                 fill = "grey", colour="black")+
  ggtitle("PostOperative OHS change\n(English Test Set 2018/19)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("Difference OHS PostOp - PreOp")+
  scale_x_continuous(limits=c(-48,48),breaks = seq(-48,48,4))+
  geom_vline(xintercept=8, colour="red", linetype = "longdash", size = 1)  +
  annotate(x=0,y=+Inf,label="MCID\nthreshold",vjust=2,geom="text", colour="red", size = 4)

## Histogram of difference [EQ-VAS PostOp - PreOp]
Hist_VAS_English.test.1819<-ggplot(English.test.1819, aes(x = VAS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1,
                 bins=20, 
                 fill = "grey", colour="black")+
  ggtitle("PostOperative EQ-VAS change\n(English Test Set 2018/19)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5,face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("Difference EQ-VAS PostOp - PreOp")+
  scale_x_continuous(limits=c(-100,100),breaks = seq(-100,100,10))+
  geom_vline(xintercept=11, colour="red", linetype = "longdash", size = 1)  + #check the threshold line is plotted at std dev*0.5
  annotate(x=30,y=+Inf,label="MCID\nthreshold",vjust=2,geom="text", colour="red", size = 4)



## Histogram of preOp OHS
Hist_preopOHS_English.test.1819<-ggplot(English.test.1819, aes(x = OHS_PREOP_TOTSCORE)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PreOperative OHS tot score\n(English Test Set 2018/19)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("PreOperative OHS total score ")+
  scale_x_continuous(limits=c(0,48),breaks = seq(0,48,4))

## Histogram of preOp EQ-VAS
Hist_preopVAS_English.test.1819<-ggplot(English.test.1819, aes(x = EQ5D_PREOP_VAS)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PreOperative EQ-VAS score\n(English Test Set 2018/19)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("PreOperative EQ-VAS score")+
  scale_x_continuous(limits=c(0,100),breaks = seq(0,100,10))



##create combined plots for relevant continuous variables (OHS tot score and EQ-VAS)
# import relevant plots from Amplitude Hips
attach("code/01_Preprocess_WelshHips.RData")
ls(2)

Hist_OHS_test.AMPLITUDE <-Hist_OHS_test.AMPLITUDE
Hist_VAS_test.AMPLITUDE <- Hist_VAS_test.AMPLITUDE
Hist_preopOHS_AMP_HIPS_CLEANED3<-Hist_preopOHS_AMP_HIPS_CLEANED3
Hist_preopVAS_AMP_HIPS_CLEANED3 <- Hist_preopVAS_AMP_HIPS_CLEANED3

library(ggpubr)
ggarrange(Hist_preopOHS_training.1618,
          Hist_OHS_training.1618,
          Hist_preopOHS_English.test.1819,
          Hist_OHS_English.test.1819,
          Hist_preopOHS_AMP_HIPS_CLEANED3,
          Hist_OHS_test.AMPLITUDE,
          ncol = 2, 
          nrow = 3)
## 1000X700 HistogramsOHS

ggarrange(Hist_preopVAS_training.1618,
          Hist_VAS_training.1618,
          Hist_preopVAS_English.test.1819,
          Hist_VAS_English.test.1819,
          Hist_preopVAS_AMP_HIPS_CLEANED3,
          Hist_VAS_test.AMPLITUDE,
          ncol = 2, 
          nrow = 3)

## 1000X700 HistogramsEQVAS_hips 
#.........................................................................................
#.........................................................................................


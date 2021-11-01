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
#.........................................................................................
#.........................................................................................

# Change column names -----------------------------------------------------
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
#.........................................................................................
#.........................................................................................


# Merge datasets ----------------------------------------------------------
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
#.........................................................................................
#.........................................................................................


# Dealing with NAs --------------------------------------------------------
#substituting *
is.na(English.dataset.hip) <- English.dataset.hip == "*" # substituting '*' with NA
#substituting 9 in the relevant columns with NAs
is.na(English.dataset.hip[,c(5:12,25:29,32:48,55:66,68:79)]) <- English.dataset.hip[,c(5:12,25:29,32:48,55:66,68:79)] == 9 # substituting 9 with NA in selected columns
#substituting 999 in the relevant columns with NAs
is.na(English.dataset.hip) <- English.dataset.hip == 999 # substituting 999 with NA in selected columns
## count N patients for each year
English.dataset.hip %>% group_by(YEAR) %>% count()
## count N missing values by column
#sapply(English.dataset.hip, function(x){sum(is.na(x))})
rm(list=ls(pattern="Eng_hip*"))
rm(hip_tables_13_20,input_folder,new_col_names_hips,tb)
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
##Check distributions of subjects by number of missing values for 
## pre and post-op OHS
English.dataset.hip %>% count(missVal_OHS_PREOP) 
English.dataset.hip %>% count(missVal_OHS_POSTOP)
## no imputation is required as no subject has 1 or 2 missing values for pre/post-op OHS
## (see Murray et al. 2007 rules)
#.........................................................................................
#.........................................................................................

# Keep only relevant years ------------------------------------------------
## only from 2012/13 onwards
English.dataset.hip_extended<-English.dataset.hip[!(English.dataset.hip$YEAR == "2011/12" |
                                                      English.dataset.hip$YEAR == "2012/13"),]
English.dataset.hip_extended %>% count(YEAR)
#.........................................................................................
#.........................................................................................

# Data cleaning -----------------------------------------------------------
##Identify variables with 0 or near-0 variance
NZV<-nearZeroVar(English.dataset.hip_extended, saveMetrics = T)
NZV[NZV$zeroVar==TRUE | NZV$nzv==TRUE ,]
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
## Remove all rows with missing values
English.dataset.hip_extended_cln4<-English.dataset.hip_extended_cln3[complete.cases(English.dataset.hip_extended_cln3),]
English.dataset.hip_extended_cln4 %>% count(YEAR)
names(English.dataset.hip_extended_cln4)
#.........................................................................................
#.........................................................................................


# Create outcome variable MCID --------------------------------------------
## calculating std deviation for preop EQ-VAS variable so that the EQ5-VAS MCID threshold
## can be identified
English.dataset.hip_extended_cln4 %>% summarise(Mean = mean(EQ5D_PREOP_VAS),
                                                stdDev = sd(EQ5D_PREOP_VAS))
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
#.........................................................................................
#.........................................................................................


# Create outcome variables OHS severity scores ----------------------------
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
#.........................................................................................
#.........................................................................................


# Demographics of the whole dataset ---------------------------------------
### for categorical variables
DescriptiveCatBasic(input_dataset = English.dataset.hip_extended_cln4, 
                    output_table = Descr1.English.dataset.hip_extended_cln4)
DescriptiveCatExtra(input_dataset = English.dataset.hip_extended_cln4, 
                    output_table = Descr2.English.dataset.hip_extended_cln4)
DescriptiveCont(input_dataset = English.dataset.hip_extended_cln4, 
                output_table = Descr3.English.dataset.hip_extended_cln4)
Descr3.English.dataset.hip_extended_cln4

# Extracting English Training set (Year = 2016/17+2017/18) + Demog --------
English.training.1618<-English.dataset.hip_extended_cln4[
  English.dataset.hip_extended_cln4$YEAR=='2016/17'|
    English.dataset.hip_extended_cln4$YEAR=='2017/18' ,]
table(English.training.1618$YEAR)
### for categorical variables
DescriptiveCatBasic(input_dataset = English.training.1618, 
                    output_table = Descr1.English.training.1618)
DescriptiveCatExtra(input_dataset = English.training.1618, 
                    output_table = Descr2.English.training.1618)
### for continous variables
DescriptiveCont2(input_dataset = English.training.1618, 
                 output_table = Descr3.English.training.1618)
write.csv(Descr1.English.training.1618,"Descr1.English.training.1618.csv", row.names = FALSE)
write.csv(Descr2.English.training.1618,"Descr2.English.training.1618.csv", row.names = FALSE)
write.csv(Descr3.English.training.1618,"Descr3.English.training.1618.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................


# Plotting distributions for relevant continuous in OHS and EQ-VAS --------
#### including MCID threshold calculated from English.training.1618 dataset
## Histogram of difference [OHS PostOp - PreOp]
Hist_OHS_training.1618<-ggplot(English.training.1618, aes(x = OHS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1,
                 #bins=20,
                 fill = "grey", colour="black")+
  ggtitle("PostOperative OHS change\n(English THA Training Set)")+
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
  ggtitle("PostOperative EQ-VAS change\n(English THA Training Set)")+
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
  geom_vline(xintercept=11, colour="red", linetype = "longdash", size = 1)  + 
  annotate(x=30,y=+Inf,label="MCID\nthreshold",vjust=2,geom="text", colour="red", size = 4)
## Histogram of preOp OHS
Hist_preopOHS_training.1618<-ggplot(English.training.1618, aes(x = OHS_PREOP_TOTSCORE)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PreOperative OHS tot score\n(English THA Training Set)")+
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
  ggtitle("PreOperative EQ-VAS score\n(English THA Training Set)")+
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
#.........................................................................................
#.........................................................................................

# Extracting English Test set (Year = 2018/19) + Demographics -------------
English.test.1819<-English.dataset.hip_extended_cln4[
  English.dataset.hip_extended_cln4$YEAR=='2018/19',]
### for categorical variables
DescriptiveCatBasic(input_dataset = English.test.1819, 
                    output_table = Descr1.English.test.1819)
DescriptiveCatExtra(input_dataset = English.test.1819, 
                    output_table = Descr2.English.test.1819)
DescriptiveCont2(input_dataset = English.test.1819, 
                 output_table = Descr3.English.test.1819)
write.csv(Descr1.English.test.1819,"Descr1.English.test.1819.csv", row.names = FALSE)
write.csv(Descr2.English.test.1819,"Descr2.English.test.1819.csv", row.names = FALSE)
write.csv(NEW_Descr3.English.test.1819,"Descr3.English.test.1819.csv", row.names = FALSE) 
#.........................................................................................
#.........................................................................................

# histograms for hips (pre and difference post-pre) -----------------------
#### Plotting distributions for relevant continuous vars in OHS and EQ-VAS
#### including MCID threshold calculated from English.training.1618 dataset
## Histogram of difference [OHS PostOp - PreOp]
Hist_OHS_English.test.1819<-ggplot(English.test.1819, aes(x = OHS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PostOperative OHS change\n(English THA Test Set)")+
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
  ggtitle("PostOperative EQ-VAS change\n(English THA Test Set)")+
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
  ggtitle("PreOperative OHS tot score\n(English THA Test Set)")+
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
  ggtitle("PreOperative EQ-VAS score\n(English THA Test Set)")+
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



# create combined plots for relevant continuous variables -----------------
##(OHS tot score and EQ-VAS)
# import relevant plots from Amplitude Hips
attach("code/01_Preprocess_WelshHips.RData")
#ls(2)
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

# Logistic regression on Test Sets (Eng vs Welsh) -------------------------
## Logistic regression using predictors to predict Welsh or English country in the Test sets only
# import relevant table from Amplitude Hips
attach("code/01_Preprocess_WelshHips.RData")
AMP_HIPS_CLEANED_3.small<-AMP_HIPS_CLEANED_3.small
## Create an extra col in both test sets for Country 
AMP_HIPS_CLEANED_3.small$COUNTRY<-"WALES"
English.test.1819$COUNTRY<-"ENGLAND"
## Union of English and AMP_HIPS_CLEANED3 test sets, keeping only relevant columns
col_to_keep_test_merge<-c("REVISION",
                          "AGEBAND",
                          "SEX",
                          "EQ5D_PREOP_INDEX",
                          "EQ5D_PREOP_VAS",
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
                          #"OHS_PREOP_TOTSCORE", ## this is correlated with all dimensions since it is their sum
                          #"OHS_MCID",
                          #"VAS_MCID",
                          "COUNTRY"
)
Dataset.LogReg.byCountry<-rbind(English.test.1819[,col_to_keep_test_merge],
                                AMP_HIPS_CLEANED_3.small[,col_to_keep_test_merge])
Dataset.LogReg.byCountry$COUNTRY<-as.factor(Dataset.LogReg.byCountry$COUNTRY)
#To specify: predicting COUNTRY = WALES
Dataset.LogReg.byCountry$COUNTRY <- relevel(Dataset.LogReg.byCountry$COUNTRY, ref = "ENGLAND")
#table(Dataset.LogReg.byCountry$COUNTRY)
##log regression
LogReg.byCountry <- glm(COUNTRY ~ ., data = Dataset.LogReg.byCountry, family = "binomial")
LogReg.byCountry_summary<-summary(LogReg.byCountry)
LogReg.byCountry_summary$coefficients
write.csv(LogReg.byCountry_summary$coefficients,"output/thesis_files/Eng_vs_Welsh_test_betas.csv", row.names = TRUE)
### calculate ORs from beta values
Eng_vs_Welsh_test_ORs<-exp(cbind(coef(LogReg.byCountry), confint(LogReg.byCountry))) 
write.csv(Eng_vs_Welsh_test_ORs,"output/thesis_files/Eng_vs_Welsh_test_ORs.csv", row.names = TRUE)
#.........................................................................................
#.........................................................................................


# Check diff distrib OHS limp and revision Welsh vs Eng test --------------
English.training.1618$COUNTRY<-"ENGLISH TRAINING"
names(AMP_HIPS_CLEANED_3.small)
col_to_keep_test_merge2<-c("REVISION",
                          "AGEBAND",
                          "SEX",
                          "EQ5D_PREOP_INDEX",
                          "EQ5D_PREOP_VAS",
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
                          "OHS_MCID",
                          "COUNTRY"
)
Dataset.LogReg.byCountry2<-rbind(English.training.1618[,col_to_keep_test_merge2],
                                 English.test.1819[,col_to_keep_test_merge2],
                                AMP_HIPS_CLEANED_3.small[,col_to_keep_test_merge2])
Dataset.LogReg.byCountry2$COUNTRY<-factor(Dataset.LogReg.byCountry2$COUNTRY, 
                                          levels = c("ENGLISH TRAINING", "ENGLAND", "WALES"))
Dataset.LogReg.byCountry2$REVISION<-factor(Dataset.LogReg.byCountry2$REVISION, 
                                          levels = c("0", "1"))
Dataset.LogReg.byCountry2 %>% group_by(COUNTRY,REVISION,OHS_MCID) %>% summarise(N = n())%>% 
  group_by(COUNTRY) %>%
  mutate(proportion = round(N/sum(N),2))%>%  
  #https://online.stat.psu.edu/stat100/lesson/9/9.1
  mutate(std_err_proportion = round((sqrt(proportion*(1-proportion)))/N,4))%>% 
  ggplot(aes(x=REVISION,y=proportion,fill = as.factor(OHS_MCID))) +
  geom_bar(stat="identity"#, position = "dodge"
           )+ 
  facet_grid(~COUNTRY, scales="free_x", space="free",
             labeller = as_labeller(c(`ENGLISH TRAINING` = "English\nTraining set",
                                      `ENGLAND` = "English\nTest set",
                                      `WALES` = "Welsh\nTest set")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_discrete(labels=c("NO", "YES"))+
  scale_y_continuous(name = "% patients",
                     labels = scales::percent, limits=c(0,1))+
  scale_fill_manual(name = "Post-surgical\nachievement\nOHS MCID",
                    values=c('darkgrey','blue'),
                    labels = c("Not achieved", "Achieved"))
Dataset.LogReg.byCountry2 %>% group_by(COUNTRY,OHS_PREOP_LIMPING,OHS_MCID) %>% summarise(N = n())%>% 
  group_by(COUNTRY) %>%
  mutate(proportion = round(N/sum(N),2))%>%  
  #https://online.stat.psu.edu/stat100/lesson/9/9.1
  mutate(std_err_proportion = round(sqrt((proportion*(1-proportion))/N),4))%>% 
  ggplot(aes(x=OHS_PREOP_LIMPING,y=proportion,fill = as.factor(OHS_MCID))) +
  geom_bar(stat="identity"#, position = "dodge"
           )+ 
  facet_grid(~COUNTRY, scales="free_x", space="free",
             labeller = as_labeller(c(`ENGLISH TRAINING` = "English\nTraining set",
                                      `ENGLAND` = "English\nTest set",
                                      `WALES` = "Welsh\nTest set")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(name = "OHS pre-operative Limping dimension")+
  scale_y_continuous(name = "% patients",
                     labels = scales::percent, limits=c(0,1))+
  scale_fill_manual(name = "Post-surgical\nachievement\nOHS MCID",
                    values=c('darkgrey','blue'),
                    labels = c("Not achieved", "Achieved")) 
#.........................................................................................
#.........................................................................................

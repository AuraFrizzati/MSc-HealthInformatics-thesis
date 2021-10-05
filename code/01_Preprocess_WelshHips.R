###### PRE-PROCESSING - Welsh Hips dataset (PRIMARY & REVISION)

### Import libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
source("code/00_ProjectFunctions.R") # library of custom-made functions
#.........................................................................................
#.........................................................................................


### Import input data -------------------------------------------------------
AMP_HIPS_PRIM<-read.csv("input_data/Welsh_data/Amplitude_HipsPrimary190404_forAuraFrizzatiMSc_anonRP210414.csv", 
                        header = T,
                        na.strings=c("", "I", "DUE", "OVERDUE", "NA"))

AMP_HIPS_REV<-read.csv("input_data/Welsh_data/Amplitude_HipsRevision170803_forAuraFrizzatiMSc_anonRP210414.csv", 
                       header = T,
                       na.strings=c("", "I", "DUE", "OVERDUE", "NA"))
#.........................................................................................
#.........................................................................................



# Variables' format &  missing values -------------------------------------

## PRIMARY UNIQUE VALUES
values_by_column(input_dataset="AMP_HIPS_PRIM", 
                 output_dir="output/extra_files/", 
                 output_name="AMP_HIPS_PRIM")

## TO FLAG AS MISSING VALUES for PRIMARY: 
# - GENDER: "I"
# - VARIOUS DATES: "DUE", "OVERDUE"
# - "NA" as a string variable

## REVISIONS UNIQUE VALUES
values_by_column(input_dataset="AMP_HIPS_REV", 
                 output_dir="output/extra_files/", 
                 output_name="AMP_HIPS_REV")
# VALUES FOR REVISIONS SEEM ALL OK (I already have age and I do not need to convert dates)
#.........................................................................................
#.........................................................................................


# Primary: choose Date field ----------------------------
### DEAL WITH DATES FOR PRIMARY DATASET: 

# CONVERT "Activity.Date", "Start.Date" and "Completed.Date...OHS.Oxford.Hip.Score...Score...Baseline"
AMP_HIPS_PRIM$ACT_DATE<-as.Date(AMP_HIPS_PRIM$Activity.Date, format = "%d/%m/%Y")
subset(AMP_HIPS_PRIM, select=c(ACT_DATE,Activity.Date)) # they look fine

AMP_HIPS_PRIM$START_DATE<-as.Date(AMP_HIPS_PRIM$Start.Date, format = "%d/%m/%Y")
subset(AMP_HIPS_PRIM, select=c(START_DATE,Start.Date)) # they look fine

AMP_HIPS_PRIM$OHS_BASELINE_DATE<-as.Date(AMP_HIPS_PRIM$Completed.Date...OHS.Oxford.Hip.Score...Score...Baseline, format = "%d/%m/%Y")
subset(AMP_HIPS_PRIM, select=c(OHS_BASELINE_DATE,Completed.Date...OHS.Oxford.Hip.Score...Score...Baseline)) # they look fine

# EXTRACT DATES AND REMOVE ROWS WITH MISSING VALUES
AMP_HIPS_PRIM_DATES<-subset(AMP_HIPS_PRIM, select=c(pID,ACT_DATE,START_DATE,OHS_BASELINE_DATE))
AMP_HIPS_PRIM_DATES<-AMP_HIPS_PRIM_DATES[complete.cases(AMP_HIPS_PRIM_DATES), ]

# CALCULATE DIFFERENCES IN DAYS: START_DATE-OHS_BASELINE_DATE
AMP_HIPS_PRIM_DATES$DATE.DIFF1<-difftime(AMP_HIPS_PRIM_DATES$START_DATE ,
                                         AMP_HIPS_PRIM_DATES$OHS_BASELINE_DATE , units = c("days"))
DATE_DIFF1<-subset(AMP_HIPS_PRIM_DATES, select=c(pID,DATE.DIFF1))
DATE_DIFF1$diff_type<-1
names(DATE_DIFF1)[names(DATE_DIFF1) == "DATE.DIFF1"] <- "DATE_DIFF"

# CALCULATE DIFFERENCES IN DAYS: ACT_DATE-OHS_BASELINE_DATE
AMP_HIPS_PRIM_DATES$DATE.DIFF2<-difftime(AMP_HIPS_PRIM_DATES$ACT_DATE ,
                                         AMP_HIPS_PRIM_DATES$OHS_BASELINE_DATE , units = c("days"))
DATE_DIFF2<-subset(AMP_HIPS_PRIM_DATES, select=c(pID,DATE.DIFF2))
DATE_DIFF2$diff_type<-2
names(DATE_DIFF2)[names(DATE_DIFF2) == "DATE.DIFF2"] <- "DATE_DIFF"

DATE_DIFF_ALL<-rbind(DATE_DIFF1,DATE_DIFF2)
rm(DATE_DIFF1,DATE_DIFF2)
## PLOT DISTRIBUTIONS OF DATE DIFFERENCES AND CHOOSE WHICH VARIABLE TO USE TO CALCULATE AGE

DATE_DIFF_ALL$DATE_DIFF<-as.integer(DATE_DIFF_ALL$DATE_DIFF) 
DATE_DIFF_ALL$diff_type<-as.factor(DATE_DIFF_ALL$diff_type)

ggplot(DATE_DIFF_ALL, aes(x=diff_type, y=DATE_DIFF)) + 
  geom_boxplot(aes(fill=factor(diff_type))) + 
  geom_point(size=0.5) +
  xlab("Date Variables") + ylab("Dates' difference [in days]") +
  ggtitle("Amplitude Hips - Primary dataset")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_discrete(labels=c("START DATE -\n OHS baseline","ACT DATE -\n OHS baseline")) +
  scale_fill_manual(values=c("darkgrey","white"))+ 
  theme(legend.position = "none")  

ggplot() + 
  geom_histogram(aes(x=DATE_DIFF_ALL[DATE_DIFF_ALL$diff_type=='1',]$DATE_DIFF, 
                     fill = DATE_DIFF_ALL[DATE_DIFF_ALL$diff_type=='1',]$diff_type), 
                 alpha = 0.8) +
  geom_histogram(aes(x=DATE_DIFF_ALL[DATE_DIFF_ALL$diff_type=='2',]$DATE_DIFF, 
                     fill = DATE_DIFF_ALL[DATE_DIFF_ALL$diff_type=='2',]$diff_type),
                 alpha = 0.7)+
  xlab("Dates' difference [in days]") + ylab("Number of subjects") +
  ggtitle("Amplitude Hips - Primary dataset")+
  scale_fill_manual(name="Date Variables", 
                    values=c("black","darkgrey"),
                    labels=c("START DATE -\n OHS baseline", "ACT DATE -\n OHS baseline"))+
  coord_cartesian(ylim = c(0, 1700),
                  xlim=c(-700, 700))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.9, 0.9))


## descriptives 
DATE_DIFF_ALL %>% group_by(diff_type) %>% summarise (Mean = mean(DATE_DIFF),
                                                     StdDev = sd(DATE_DIFF),
                                                     Min = min(DATE_DIFF),
                                                     Max = max(DATE_DIFF))

## ACTIVITY DATE chosen as main date variable to calculate patients' age at surgery 
## (much less spread around baseline date)
rm(AMP_HIPS_PRIM_DATES,DATE_DIFF_ALL)
#.........................................................................................
#.........................................................................................


# Primary: patients 2+ primary surgeries on the same side--------
AMP_HIPS_PRIM %>% 
  group_by(pID,Pathway.Side) %>% 
  summarise (N_Primary_sameSide = n()) %>%
  arrange(-N_Primary_sameSide)

AMP_HIPS_PRIM %>% 
  group_by(pID,Pathway.Side) %>% 
  summarise (N_Primary_sameSide = n()) %>%
  group_by(N_Primary_sameSide) %>%
  summarise (Tot = n())

## Extract list of subjects with N_Primary_sameSide > 1
remove<-as.data.frame(AMP_HIPS_PRIM %>% 
                        group_by(pID,Pathway.Side) %>% 
                        summarise (N_Primary_sameSide = n()) %>% 
                        filter(N_Primary_sameSide > 1)%>% 
                        unite(excl_filter, c("pID", "Pathway.Side"), sep = ""))

### remove subjects with N_Primary_sameSide > 1 
AMP_HIPS_PRIM$combID<-paste0(AMP_HIPS_PRIM$pID, AMP_HIPS_PRIM$Pathway.Side)

AMP_HIPS_PRIM.cleaned<-AMP_HIPS_PRIM[!(AMP_HIPS_PRIM$combID %in% remove$excl_filter),]
rm(remove)
#.........................................................................................
#.........................................................................................


# Primary: create Age variable --------------------------------------------
### create Patient AGE variable: Activity.Date - Year.of.birth
AMP_HIPS_PRIM.cleaned$Activity.Year<-as.integer(
  substring(AMP_HIPS_PRIM.cleaned$Activity.Date,7,10))


AMP_HIPS_PRIM.cleaned$Age<-AMP_HIPS_PRIM.cleaned$Activity.Year-
  AMP_HIPS_PRIM.cleaned$Year.of.birth

## visually checking Age distribution:

ggplot(AMP_HIPS_PRIM.cleaned, aes(x=Age)) + geom_histogram()
# there is a patient aged 1, might have to double-check this 
# again after I have removed all NAs
#.........................................................................................
#.........................................................................................


# Primary: re-organise columns ------------------------------------------------------
### drop  unnecessary columns
drop_columns<-c("Pathway.Side",
                "Year.of.birth",
                "Activity.Date",
                "Start.Date",
                "Completed.Date...OHS.Oxford.Hip.Score...Score...Baseline",
                "Completed.Date...OHS.Oxford.Hip.Score...Score...6.Months",
                "Completed.Date...OHS.Oxford.Hip.Score...Score...12.Months",
                "Completed.Date...EQ.5D.5L...Health.VAS...Baseline",
                "Completed.Date...EQ.5D.5L...Health.VAS...6.Months",
                "Completed.Date...EQ.5D.5L...Health.VAS...12.Months",
                "Completed.Date...EQ.5D.5L...Index...Baseline",
                "Completed.Date...EQ.5D.5L...Index...6.Months",
                "EQ.5D.5L...Index...6.Months",
                "Completed.Date...EQ.5D.5L...Health.VAS...12.Months.1",
                "EQ.5D.5L...Index...12.Months",
                "EQ.5D.5L.Baseline...b.ANXIETY.DEPRESSION..b.",
                "EQ.5D.5L.Baseline...b.MOBILITY..b..",
                "EQ.5D.5L.Baseline...b.PAIN.DISCOMFORT..b.",
                "EQ.5D.5L.Baseline...b.SELF.CARE..b.",
                "EQ.5D.5L.Baseline...b.USUAL.ACTIVITIES..b...eg..work..study..housework..family.or.leisure.activities.",
                "ACT_DATE",
                "START_DATE",
                "OHS_BASELINE_DATE",
                "combID",
                "Activity.Year")

AMP_HIPS_PRIM.cleaned2<-
  AMP_HIPS_PRIM.cleaned[ , 
                         !(names(AMP_HIPS_PRIM.cleaned) %in% drop_columns)]
# from 42 to 23 variables

## re-name columns 
new_col_names<-c("pID",
                 "REVISION",
                 "SEX",
                 "OHS_PREOP_TOTSCORE",
                 "OHS_POSTOP6M_TOTSCORE",
                 "OHS_POSTOP12M_TOTSCORE",
                 "EQ5D_PREOP_VAS",
                 "EQ5D_POSTOP6M_VAS",
                 "EQ5D_POSTOP12M_VAS",
                 "EQ5D_PREOP_INDEX",
                 "OHS_PREOP_SHOPPING",
                 "OHS_PREOP_STANDING",
                 "OHS_PREOP_WALKING",
                 "OHS_PREOP_STAIRS",
                 "OHS_PREOP_DRESSING",
                 "OHS_PREOP_LIMPING",
                 "OHS_PREOP_NIGHTPAIN",
                 "OHS_PREOP_SUDDENPAIN",
                 "OHS_PREOP_TRANSPORT",
                 "OHS_PREOP_WASHING",
                 "OHS_PREOP_WORK",
                 "OHS_PREOP_PAIN",
                 # "EQ5D5L_PREOP_ANXIETY",
                 # "EQ5D5L_PREOP_MOBILITY",
                 # "EQ5D5L_PREOP_DISCOMFORT",
                 # "EQ5D5L_PREOP_SELFCARE",
                 # "EQ5D5L_PREOP_ACTIVITY",
                 "AGE")

# extract df columns' original names and add new columns' names 
colnames_orig_and_new <- cbind(as.data.frame(colnames(AMP_HIPS_PRIM.cleaned2)), new_col_names)

# rename the columns of the original df with the new names:
names(AMP_HIPS_PRIM.cleaned2)[match(colnames_orig_and_new[,1], 
                                    names(AMP_HIPS_PRIM.cleaned2))] = colnames_orig_and_new[,2]

rm(drop_columns,new_col_names,colnames_orig_and_new, AMP_HIPS_PRIM.cleaned)
#.........................................................................................
#.........................................................................................


# Primary: missing values baseline OHS and EQ-VAS ----------------------------------
## check N of missing values for the baseline OHS and EQ-VAS scores

AMP_HIPS_PRIM.cleaned2$BASELINE_SCORE_TYPE<-NA
AMP_HIPS_PRIM.cleaned2[!is.na(AMP_HIPS_PRIM.cleaned2$OHS_PREOP_TOTSCORE)&
                         is.na(AMP_HIPS_PRIM.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"OHSPREOP_ONLY"
AMP_HIPS_PRIM.cleaned2[!is.na(AMP_HIPS_PRIM.cleaned2$OHS_PREOP_TOTSCORE)&
                         !is.na(AMP_HIPS_PRIM.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"OHSPREOP&VASPREOP"
AMP_HIPS_PRIM.cleaned2[is.na(AMP_HIPS_PRIM.cleaned2$OHS_PREOP_TOTSCORE)&
                         !is.na(AMP_HIPS_PRIM.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"VASPREOP_ONLY"


AMP_HIPS_PRIM.cleaned2 %>% group_by(BASELINE_SCORE_TYPE) %>% summarise (tot = n())

## remove subjects without both OHS and VAS pre-op:
AMP_HIPS_PRIM.cleaned3<-AMP_HIPS_PRIM.cleaned2[AMP_HIPS_PRIM.cleaned2$BASELINE_SCORE_TYPE=="OHSPREOP&VASPREOP"&
                                                 !is.na(AMP_HIPS_PRIM.cleaned2$BASELINE_SCORE_TYPE),]

##checking N of missing values/column
#sapply(AMP_HIPS_PRIM.cleaned3, function(x){sum(is.na(x))})

## remove 1 subject without all pre-op OHS dimensions:
AMP_HIPS_PRIM.cleaned3<-AMP_HIPS_PRIM.cleaned3[!is.na(AMP_HIPS_PRIM.cleaned3$OHS_PREOP_SHOPPING),]

##checking N of missing values/column
#sapply(AMP_HIPS_PRIM.cleaned3, function(x){sum(is.na(x))})

## After these cleaning steps, only these predictors have missing values:
## OHS_POSTOP6M_TOTSCORE, OHS_POSTOP12M_TOTSCORE, EQ5D_POSTOP6M_VAS,  EQ5D_POSTOP12M_VAS

rm(AMP_HIPS_PRIM.cleaned2)
#.........................................................................................
#.........................................................................................


# Primary: remove cases without tot OHS 6 or 12 months --------------------
## check combinations of OHS TOT SCORE 6-months and 12-months

AMP_HIPS_PRIM.cleaned3$OHS_POSTOP_TOTSCORE_TYPE<-NA
AMP_HIPS_PRIM.cleaned3[!is.na(AMP_HIPS_PRIM.cleaned3$OHS_POSTOP6M_TOTSCORE)&
                         is.na(AMP_HIPS_PRIM.cleaned3$OHS_POSTOP12M_TOTSCORE),]$OHS_POSTOP_TOTSCORE_TYPE<-"6MONTHS_ONLY"
AMP_HIPS_PRIM.cleaned3[!is.na(AMP_HIPS_PRIM.cleaned3$OHS_POSTOP6M_TOTSCORE)&
                         !is.na(AMP_HIPS_PRIM.cleaned3$OHS_POSTOP12M_TOTSCORE),]$OHS_POSTOP_TOTSCORE_TYPE<-"6MONTHS&12MONTHS"
AMP_HIPS_PRIM.cleaned3[is.na(AMP_HIPS_PRIM.cleaned3$OHS_POSTOP6M_TOTSCORE)&
                         !is.na(AMP_HIPS_PRIM.cleaned3$OHS_POSTOP12M_TOTSCORE),]$OHS_POSTOP_TOTSCORE_TYPE<-"12MONTHS_ONLY"


AMP_HIPS_PRIM.cleaned3 %>% group_by(OHS_POSTOP_TOTSCORE_TYPE) %>% summarise (tot = n())

#************
### remove rows without OHS 6-month post-op & without OHS 12-month post-op
AMP_HIPS_PRIM.cleaned4<-AMP_HIPS_PRIM.cleaned3[!is.na(AMP_HIPS_PRIM.cleaned3$OHS_POSTOP_TOTSCORE_TYPE),]

rm(AMP_HIPS_PRIM.cleaned3)
#.........................................................................................
#.........................................................................................



# Primary: remove cases without EQVAS 6 or 12 months --------------------

## check combinations of EQ-VAS 6-months and 12-months
AMP_HIPS_PRIM.cleaned4$EQVAS_POSTOP_TYPE<-NA
AMP_HIPS_PRIM.cleaned4[!is.na(AMP_HIPS_PRIM.cleaned4$EQ5D_POSTOP6M_VAS)&
                         is.na(AMP_HIPS_PRIM.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"6MONTHS_ONLY"
AMP_HIPS_PRIM.cleaned4[!is.na(AMP_HIPS_PRIM.cleaned4$EQ5D_POSTOP6M_VAS)&
                         !is.na(AMP_HIPS_PRIM.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"6MONTHS&12MONTHS"
AMP_HIPS_PRIM.cleaned4[is.na(AMP_HIPS_PRIM.cleaned4$EQ5D_POSTOP6M_VAS)&
                         !is.na(AMP_HIPS_PRIM.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"12MONTHS_ONLY"
AMP_HIPS_PRIM.cleaned4 %>% group_by(EQVAS_POSTOP_TYPE) %>% summarise (tot = n())

### remove rows without EQ-VAS 6-month post-op & without EQ-VAS 12-month post-op
AMP_HIPS_PRIM.cleaned5<-AMP_HIPS_PRIM.cleaned4[!is.na(AMP_HIPS_PRIM.cleaned4$EQVAS_POSTOP_TYPE),]

rm(AMP_HIPS_PRIM.cleaned4)
#.........................................................................................
#.........................................................................................



# Revision: patients 2+ primary surgeries on the same side--------
### check for the presence of patients with multiple revision surgeries on the same side  (although these might be plausible)
AMP_HIPS_REV %>% 
  group_by(pID,Pathway.Side) %>% 
  summarise (N_Revision_sameSide = n()) %>%
  arrange(-N_Revision_sameSide)

AMP_HIPS_REV %>% 
  group_by(pID,Pathway.Side) %>% 
  summarise (N_Revision_sameSide = n()) %>%
  group_by(N_Revision_sameSide) %>%
  summarise (Tot = n())

# N_Revision_sameSide   Tot
# 1                     277
# 2                       7 --> 14 records, I remove them since their dates are very close and some have different scores

## Extract list of subjects with N_Revision_sameSide > 1
remove<-as.data.frame(AMP_HIPS_REV %>% 
                        group_by(pID,Pathway.Side) %>% 
                        summarise (N_Revision_sameSide = n()) %>% 
                        filter(N_Revision_sameSide > 1)%>% 
                        unite(excl_filter, c("pID", "Pathway.Side"), sep = ""))


#************
### remove subjects with N_Revision_sameSide > 1 
AMP_HIPS_REV$combID<-paste0(AMP_HIPS_REV$pID, AMP_HIPS_REV$Pathway.Side)

AMP_HIPS_REV.cleaned<-AMP_HIPS_REV[!(AMP_HIPS_REV$combID %in% remove$excl_filter),]
rm(remove)
#.........................................................................................
#.........................................................................................




# Revision: re-organise columns ------------------------------------------------------
### drop  unnecessary columns
drop_columns<-c("Pathway.Side",
                "Activity.Date",
                "Start.Date1",
                "Completed.Date...Oxford.Hip.Score...Score...Baseline",
                "Completed.Date...Oxford.Hip.Score...Score...6.Months",
                "Completed.Date...Oxford.Hip.Score...Score...12.Months",
                "Completed.Date...EQ.5D.5L...Health.VAS...Baseline",
                "Completed.Date...EQ.5D.5L...Health.VAS...6.Months",
                "Completed.Date...EQ.5D.5L...Health.VAS...12.Months",
                "Completed.Date...EQ.5D.5L...Index...Baseline",
                "Completed.Date...EQ.5D.5L...Index...6.Months",
                "EQ.5D.5L...Index...6.Months",
                "Completed.Date...EQ.5D.5L...Health.VAS...12.Months.1",
                "EQ.5D.5L...Index...12.Months",
                "EQ.5D.5L.Baseline...b.ANXIETY.DEPRESSION..b.",
                "EQ.5D.5L.Baseline...b.MOBILITY..b..",
                "EQ.5D.5L.Baseline...b.PAIN.DISCOMFORT..b.",
                "EQ.5D.5L.Baseline...b.SELF.CARE..b.",
                "EQ.5D.5L.Baseline...b.USUAL.ACTIVITIES..b...eg..work..study..housework..family.or.leisure.activities.",
                "combID")

AMP_HIPS_REV.cleaned2<-
  AMP_HIPS_REV.cleaned[ , 
                        !(names(AMP_HIPS_REV.cleaned) %in% drop_columns)]

# from 43 to 23 variables
#names(AMP_HIPS_REV.cleaned2)

## re-name columns 
new_col_names<-c("pID",
                 "REVISION",
                 "SEX",
                 "AGE",
                 "OHS_PREOP_TOTSCORE",
                 "OHS_POSTOP6M_TOTSCORE",
                 "OHS_POSTOP12M_TOTSCORE",
                 "EQ5D_PREOP_VAS",
                 "EQ5D_POSTOP6M_VAS",
                 "EQ5D_POSTOP12M_VAS",
                 "EQ5D_PREOP_INDEX",
                 "OHS_PREOP_SHOPPING",
                 "OHS_PREOP_STANDING",
                 "OHS_PREOP_WALKING",
                 "OHS_PREOP_STAIRS",
                 "OHS_PREOP_DRESSING",
                 "OHS_PREOP_LIMPING",
                 "OHS_PREOP_NIGHTPAIN",
                 "OHS_PREOP_SUDDENPAIN",
                 "OHS_PREOP_TRANSPORT",
                 "OHS_PREOP_WASHING",
                 "OHS_PREOP_WORK",
                 "OHS_PREOP_PAIN")


# extract df columns' original names and add new columns' names 
colnames_orig_and_new <- cbind(as.data.frame(colnames(AMP_HIPS_REV.cleaned2)), new_col_names)

# rename the columns of the original df with the new names:
names(AMP_HIPS_REV.cleaned2)[match(colnames_orig_and_new[,1], 
                                   names(AMP_HIPS_REV.cleaned2))] = colnames_orig_and_new[,2]

rm(drop_columns,new_col_names,colnames_orig_and_new, AMP_HIPS_REV.cleaned)
#.........................................................................................
#.........................................................................................



# Revision: missing values baseline OHS and EQ-VAS ----------------------------------
## check N of missing values for the baseline OHS and EQ-VAS scores

AMP_HIPS_REV.cleaned2$BASELINE_SCORE_TYPE<-NA
AMP_HIPS_REV.cleaned2[!is.na(AMP_HIPS_REV.cleaned2$OHS_PREOP_TOTSCORE)&
                        is.na(AMP_HIPS_REV.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"OHSPREOP_ONLY"
AMP_HIPS_REV.cleaned2[!is.na(AMP_HIPS_REV.cleaned2$OHS_PREOP_TOTSCORE)&
                        !is.na(AMP_HIPS_REV.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"OHSPREOP&VASPREOP"
AMP_HIPS_REV.cleaned2[is.na(AMP_HIPS_REV.cleaned2$OHS_PREOP_TOTSCORE)&
                        !is.na(AMP_HIPS_REV.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"VASPREOP_ONLY"


AMP_HIPS_REV.cleaned2 %>% group_by(BASELINE_SCORE_TYPE) %>% summarise (tot = n())

## remove subjects without both OHS and VAS pre-op:
AMP_HIPS_REV.cleaned3<-AMP_HIPS_REV.cleaned2[AMP_HIPS_REV.cleaned2$BASELINE_SCORE_TYPE=="OHSPREOP&VASPREOP"&
                                               !is.na(AMP_HIPS_REV.cleaned2$BASELINE_SCORE_TYPE),]

##checking N of missing values/column
#sapply(AMP_HIPS_REV.cleaned3, function(x){sum(is.na(x))})

rm(AMP_HIPS_REV.cleaned2)
#.........................................................................................
#.........................................................................................



## After these cleaning steps, only these predictors have missing values:
## OHS_POSTOP6M_TOTSCORE, OHS_POSTOP12M_TOTSCORE, EQ5D_POSTOP6M_VAS,  EQ5D_POSTOP12M_VAS


# Revision: remove cases without tot OHS 6 or 12 months --------------------
## check combinations of OHS TOT SCORE 6-months and 12-months

AMP_HIPS_REV.cleaned3$OHS_POSTOP_TOTSCORE_TYPE<-NA
AMP_HIPS_REV.cleaned3[!is.na(AMP_HIPS_REV.cleaned3$OHS_POSTOP6M_TOTSCORE)&
                        is.na(AMP_HIPS_REV.cleaned3$OHS_POSTOP12M_TOTSCORE),]$OHS_POSTOP_TOTSCORE_TYPE<-"6MONTHS_ONLY"
AMP_HIPS_REV.cleaned3[!is.na(AMP_HIPS_REV.cleaned3$OHS_POSTOP6M_TOTSCORE)&
                        !is.na(AMP_HIPS_REV.cleaned3$OHS_POSTOP12M_TOTSCORE),]$OHS_POSTOP_TOTSCORE_TYPE<-"6MONTHS&12MONTHS"
AMP_HIPS_REV.cleaned3[is.na(AMP_HIPS_REV.cleaned3$OHS_POSTOP6M_TOTSCORE)&
                        !is.na(AMP_HIPS_REV.cleaned3$OHS_POSTOP12M_TOTSCORE),]$OHS_POSTOP_TOTSCORE_TYPE<-"12MONTHS_ONLY"


AMP_HIPS_REV.cleaned3 %>% group_by(OHS_POSTOP_TOTSCORE_TYPE) %>% summarise (tot = n())

#************
### remove rows without OHS 6-month post-op & without OHS 12-month post-op
AMP_HIPS_REV.cleaned4<-AMP_HIPS_REV.cleaned3[!is.na(AMP_HIPS_REV.cleaned3$OHS_POSTOP_TOTSCORE_TYPE),]

rm(AMP_HIPS_REV.cleaned3)
#.........................................................................................
#.........................................................................................


# Revision: remove cases without EQVAS 6 or 12 months --------------------

## check combinations of EQ-VAS 6-months and 12-months
AMP_HIPS_REV.cleaned4$EQVAS_POSTOP_TYPE<-NA
AMP_HIPS_REV.cleaned4[!is.na(AMP_HIPS_REV.cleaned4$EQ5D_POSTOP6M_VAS)&
                        is.na(AMP_HIPS_REV.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"6MONTHS_ONLY"
AMP_HIPS_REV.cleaned4[!is.na(AMP_HIPS_REV.cleaned4$EQ5D_POSTOP6M_VAS)&
                        !is.na(AMP_HIPS_REV.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"6MONTHS&12MONTHS"
AMP_HIPS_REV.cleaned4[is.na(AMP_HIPS_REV.cleaned4$EQ5D_POSTOP6M_VAS)&
                        !is.na(AMP_HIPS_REV.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"12MONTHS_ONLY"

AMP_HIPS_REV.cleaned4 %>% group_by(EQVAS_POSTOP_TYPE) %>% summarise (tot = n())

#************
### remove rows without EQ-VAS 6-month post-op & without EQ-VAS 12-month post-op
AMP_HIPS_REV.cleaned5<-AMP_HIPS_REV.cleaned4[!is.na(AMP_HIPS_REV.cleaned4$EQVAS_POSTOP_TYPE),]

rm(AMP_HIPS_REV.cleaned4)
#.........................................................................................
#.........................................................................................



# Merge Primary & Revision ------------------------------------------------

## check the two datasets' columns
names(AMP_HIPS_PRIM.cleaned5)
names(AMP_HIPS_REV.cleaned5)
# same columns in both datasets, although in different order (AGE)

##vertically merge two datasets' columns
AMP_HIPS_CLEANED<-rbind(AMP_HIPS_PRIM.cleaned5,AMP_HIPS_REV.cleaned5)

rm(AMP_HIPS_PRIM.cleaned5,AMP_HIPS_REV.cleaned5)
#.........................................................................................
#.........................................................................................


# Compare tot postop OHS 6 vs 12 months -----------------------------------

### Compare distributions of post-op OHS at 6 and 12 months

#extract 6 months
OHS_POSTOP6M_TOTSCORE.p<-as.data.frame(AMP_HIPS_CLEANED$OHS_POSTOP6M_TOTSCORE)
OHS_POSTOP6M_TOTSCORE.p$postTime<-'6M'
names(OHS_POSTOP6M_TOTSCORE.p)<-c('OHS','postTime')
OHS_POSTOP6M_TOTSCORE.p<-OHS_POSTOP6M_TOTSCORE.p[!is.na(OHS_POSTOP6M_TOTSCORE.p$OHS),] ##442

#extract 12 months
OHS_POSTOP12M_TOTSCORE.p<-as.data.frame(AMP_HIPS_CLEANED$OHS_POSTOP12M_TOTSCORE)
OHS_POSTOP12M_TOTSCORE.p$postTime<-'12M'
names(OHS_POSTOP12M_TOTSCORE.p)<-c('OHS','postTime')
OHS_POSTOP12M_TOTSCORE.p<-OHS_POSTOP12M_TOTSCORE.p[!is.na(OHS_POSTOP12M_TOTSCORE.p$OHS),] ##711

#bind
OHS_POSTOP_TOTSCORE.p<-rbind(OHS_POSTOP6M_TOTSCORE.p,OHS_POSTOP12M_TOTSCORE.p)
rm(OHS_POSTOP6M_TOTSCORE.p,OHS_POSTOP12M_TOTSCORE.p)

OHS_POSTOP_TOTSCORE.p$postTime<-factor(OHS_POSTOP_TOTSCORE.p$postTime,
                                       levels = c("6M", "12M"))

## visually checking OHS distributions (post op 6 vs 12 months):
ggplot() + 
  geom_histogram(aes(x=OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=='6M',]$OHS, 
                     fill = OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=='6M',]$postTime), 
                 alpha = 0.5) +
  geom_histogram(aes(x=OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=='12M',]$OHS, 
                     fill = OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=='12M',]$postTime),
                 alpha = 0.5)+
  xlab("post-op OHS tot score") + ylab("Number of subjects") +
  ggtitle("Amplitude Hips - Primary+Revision dataset")+
  scale_fill_manual(name="post-op OHS variables", 
                    values=c("red","green"),
                    labels=c("6 months", "12 months"))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.9, 0.9))

boxplot_prim_vs_rev_OHS <- ggplot(OHS_POSTOP_TOTSCORE.p, aes(x=postTime, y=OHS)) + 
  geom_boxplot(aes(fill=factor(postTime))) +  
  #stat_summary(geom = "errorbar", fun.min = mean, fun = mean, fun.max = mean, 
  #             width = .75, color = "red")+ ## add mean to box plots
  geom_point(size=0.5) +
  xlab("Collection time of post-operative OHS questionnaire") + 
  ylab("Post-operative OHS total score") +
  ggtitle("Welsh Hip Dataset - OHS")+ ##primary + revision
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c("6 months post-surgery","12 months post-surgery")) +
  scale_fill_manual(values=c("darkgrey","white"))+ 
  theme(legend.position = "none")+ ylim(0,55)+
  scale_y_continuous(breaks = seq(0,50, by = 5))

## print image
png(filename="output/thesis_files/boxplot_prim_vs_rev_OHS.png",  width = 473, height = 363)
boxplot_prim_vs_rev_OHS
dev.off()

OHS_POSTOP_TOTSCORE.p %>% group_by(postTime) %>% summarise (Mean = mean(OHS),
                                                            StdDev = sd(OHS),
                                                            Min = min(OHS),
                                                            Max = max(OHS),
                                                            Median = median(OHS),
                                                            FirstQuartile = quantile(OHS, probs = 0.25),
                                                            ThirdQuartile = quantile(OHS, probs = 0.75))

# postTime  Mean StdDev   Min   Max Median FirstQuartile ThirdQuartile
# 6M        36.7   11.1     3    48     40          32.2            45
# 12M       38.0   10.5     2    48     41          33              47

g1<-ggplot(OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=="6M",], 
           aes(sample=OHS))+
  stat_qq(shape=1) + 
  stat_qq_line(fullrange = FALSE) +
  ggtitle("QQ-plot 6-month post-operative\nOHS tot scores (Welsh Hip Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14)) +
  ylab("Sample quantiles") +
  xlab("Theoretical quantiles")

g2<-ggplot(OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=="12M",], 
           aes(sample=OHS))+
  stat_qq(shape=1) + 
  stat_qq_line(fullrange = FALSE) +
  ggtitle("QQ-plot 12-month post-operative\nOHS tot scores (Welsh Hip Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14)) +
  ylab("Sample quantiles") +
  xlab("Theoretical quantiles")

g3<-ggplot(OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=="6M",], 
           aes(x = OHS)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("6-month post-operative OHS tot scores\n(Welsh Hip Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("6-month post-operative OHS total score")+
  scale_x_continuous(limits=c(0,48),breaks = seq(0,48,4))+
  geom_vline(xintercept=mean(OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=="6M",]$OHS), colour="red", linetype = "longdash", size = 1)+
  geom_vline(xintercept=median(OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=="6M",]$OHS), colour="black", size = 1)

g4<-ggplot(OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=="12M",], 
           aes(x = OHS)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("12-month post-operative OHS tot scores\n(Welsh Hip Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("12-month post-operative OHS total score")+
  scale_x_continuous(limits=c(0,48),breaks = seq(0,48,4))+
  geom_vline(xintercept=mean(OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=="12M",]$OHS), colour="red", linetype = "longdash", size = 1)+
  geom_vline(xintercept=median(OHS_POSTOP_TOTSCORE.p[OHS_POSTOP_TOTSCORE.p$postTime=="12M",]$OHS), colour="black", size = 1)

ggpubr::ggarrange(g1,g3,g2,g4)

with(OHS_POSTOP_TOTSCORE.p, shapiro.test(OHS[postTime == "6M"]))
# W = 0.85799, p-value < 2.2e-16 --> NOT NORMALLY DISTRIBUTED, however sample size > 30

with(OHS_POSTOP_TOTSCORE.p, shapiro.test(OHS[postTime == "12M"]))
# W = 0.85421, p-value < 2.2e-16 --> NOT NORMALLY DISTRIBUTED,

## non parametric test: Wilcoxon rank sum (or Mann-Whitney) test:
wilcox.test(OHS ~ postTime, data = OHS_POSTOP_TOTSCORE.p, exact = FALSE)
# Wilcoxon rank sum test with continuity correction
# data:  OHS by postTime
# W = 144736, p-value = 0.02384
# alternative hypothesis: true location shift is not equal to 0

AMP_HIPS_CLEANED %>% group_by(OHS_POSTOP_TOTSCORE_TYPE) %>% summarise (tot = n())
# OHS_POSTOP_TOTSCORE_TYPE   tot
# * <chr>                    <int>
# 12MONTHS_ONLY              384 --> remove
# 6MONTHS&12MONTHS           327
# 6MONTHS_ONLY               115
### I will need to remove subjects with post-op OHS 12MONTHS_ONLY

rm(g1,g3,g2,g4,OHS_POSTOP_TOTSCORE.p)
#.........................................................................................
#.........................................................................................


# Compare postop EQVAS 6 vs 12 months -----------------------------------

#### Compare distributions of post-op EQ-VAS at 6 and 12 months

#extract 6 months
EQVAS_POSTOP6M.p<-as.data.frame(AMP_HIPS_CLEANED$EQ5D_POSTOP6M_VAS)
EQVAS_POSTOP6M.p$postTime<-'6M'
names(EQVAS_POSTOP6M.p)<-c('EQVAS','postTime')
EQVAS_POSTOP6M.p<-EQVAS_POSTOP6M.p[!is.na(EQVAS_POSTOP6M.p$EQVAS),] ##454

#extract 12 months
EQVAS_POSTOP12M.p<-as.data.frame(AMP_HIPS_CLEANED$EQ5D_POSTOP12M_VAS)
EQVAS_POSTOP12M.p$postTime<-'12M'
names(EQVAS_POSTOP12M.p)<-c('EQVAS','postTime')
EQVAS_POSTOP12M.p<-EQVAS_POSTOP12M.p[!is.na(EQVAS_POSTOP12M.p$EQVAS),] ##712

#bind
EQVAS_POSTOP.p<-rbind(EQVAS_POSTOP6M.p,EQVAS_POSTOP12M.p)

EQVAS_POSTOP.p$postTime<-factor(EQVAS_POSTOP.p$postTime,
                                levels = c("6M", "12M"))
## visually checking OHS distributions (post op 6 vs 12 months):
ggplot() + 
  geom_histogram(aes(x=EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=='6M',]$EQVAS, 
                     fill = EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=='6M',]$postTime), 
                 alpha = 0.5) +
  geom_histogram(aes(x=EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=='12M',]$EQVAS, 
                     fill = EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=='12M',]$postTime),
                 alpha = 0.5)+
  xlab("post-op EQ-VAS tot score") + ylab("Number of subjects") +
  ggtitle("Amplitude Hips - Primary+Revision dataset")+
  scale_fill_manual(name="post-op EQ-VAS variables", 
                    values=c("red","green"),
                    labels=c("6 months", "12 months"))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.9, 0.9))

boxplot_prim_vs_rev_EQVAS<-ggplot(EQVAS_POSTOP.p, aes(x=postTime, y=EQVAS)) + 
  geom_boxplot(aes(fill=factor(postTime))) + 
  geom_point(size=0.5) +  
  #stat_summary(geom = "errorbar", fun.min = mean, fun = mean, fun.max = mean, 
  #             width = .75, color = "red")+ ## add mean to box plots
  xlab("Collection time of post-operative EQ-VAS") + 
  ylab("Post-operative EQ-VAS score") +
  ggtitle("Welsh Hip Dataset - EQ-VAS")+ ##primary + revision
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c("6 months","12 months")) +
  scale_fill_manual(values=c("darkgrey","white"))+ 
  theme(legend.position = "none")+ ylim(0,55)+
  scale_y_continuous(breaks = seq(0,110, by = 10))

## print image
png(filename="output/thesis_files/boxplot_prim_vs_rev_EQVAS.png",  width = 473, height = 363)
boxplot_prim_vs_rev_EQVAS
dev.off()


EQVAS_POSTOP.p %>% group_by(postTime) %>% summarise (Mean = mean(EQVAS),
                                                     StdDev = sd(EQVAS),
                                                     Min = min(EQVAS),
                                                     Max = max(EQVAS),
                                                     Median = median(EQVAS),
                                                     FirstQuartile = quantile(EQVAS, probs = 0.25),
                                                     ThirdQuartile = quantile(EQVAS, probs = 0.75))


# postTime   Mean StdDev   Min   Max Median FirstQuartile ThirdQuartile
# 6M         72.1   21.7     0   100     79            60            90
# 12M        71.6   22.9     0   100     80            60            90

g5<-ggplot(EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=="6M",], 
           aes(sample=EQVAS))+
  stat_qq(shape=1) + 
  stat_qq_line(fullrange = FALSE) +
  ggtitle("QQ-plot 6-month post-operative\nEQ-VAS tot scores (Welsh Hip Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14)) +
  ylab("Sample quantiles") +
  xlab("Theoretical quantiles")

g6<-ggplot(EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=="12M",], 
           aes(sample=EQVAS))+
  stat_qq(shape=1) + 
  stat_qq_line(fullrange = FALSE) +
  ggtitle("QQ-plot 12-month post-operative\nEQ-VAS tot scores (Welsh Hip Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14)) +
  ylab("Sample quantiles") +
  xlab("Theoretical quantiles")

g7<-ggplot(EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=="6M",], 
           aes(x = EQVAS)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("6-month post-operative EQ-VAS tot scores\n(Welsh Hip Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("6-month post-operative EQ-VAS total score")+
  scale_x_continuous(limits=c(0,100),breaks = seq(0,100,4))+
  geom_vline(xintercept=mean(EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=="6M",]$EQVAS), colour="red", linetype = "longdash", size = 1)+
  geom_vline(xintercept=median(EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=="6M",]$EQVAS), colour="black", size = 1)

g8<-ggplot(EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=="12M",], 
           aes(x = EQVAS)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("12-month post-operative EQ-VAS tot scores\n(Welsh Hip Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("12-month post-operative EQ-VAS total score")+
  scale_x_continuous(limits=c(0,100),breaks = seq(0,100,4))+
  geom_vline(xintercept=mean(EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=="12M",]$EQVAS), colour="red", linetype = "longdash", size = 1)+
  geom_vline(xintercept=median(EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=="12M",]$EQVAS), colour="black", size = 1)

ggpubr::ggarrange(g5,g6,g7,g8)

with(EQVAS_POSTOP.p, shapiro.test(EQVAS[postTime == "6M"]))
# W = 0.89717, p-value < 2.2e-16 --> NOT NORMALLY DISTRIBUTED

with(EQVAS_POSTOP.p, shapiro.test(EQVAS[postTime == "12M"]))
# W = 0.88808, p-value < 2.2e-16 --> NOT NORMALLY DISTRIBUTED

## non parametric test: Wilcoxon rank sum (or Mann-Whitney) test:
wilcox.test(EQVAS ~ postTime, data = EQVAS_POSTOP.p, exact = FALSE)
# Wilcoxon rank sum test with continuity correction
# data:  EQVAS by postTime
# W = 162088, p-value = 0.9341
# alternative hypothesis: true location shift is not equal to 0

AMP_HIPS_CLEANED %>% group_by(EQVAS_POSTOP_TYPE) %>% summarise (tot = n())
# EQVAS_POSTOP_TYPE   tot
# 12MONTHS_ONLY       372
# 6MONTHS&12MONTHS    340
# 6MONTHS_ONLY        114

rm(g5,g6,g7,g8,EQVAS_POSTOP.p, EQVAS_POSTOP12M.p, EQVAS_POSTOP6M.p)
#.........................................................................................
#.........................................................................................


# Create OHS_POSTOP_TOTSCORE var ------------------------------------------
### Create OHS_POSTOP_TOTSCORE variable (use post-op 6months)
AMP_HIPS_CLEANED_2<-AMP_HIPS_CLEANED
AMP_HIPS_CLEANED_2$OHS_POSTOP_TOTSCORE<-AMP_HIPS_CLEANED$OHS_POSTOP6M_TOTSCORE 
#.........................................................................................
#.........................................................................................


# Create EQ5D_POSTOP_VAS var ------------------------------------------
#************
### Create EQ5D_POSTOP_VAS variable (use post-op 6months, if not present use post-op  12months)
AMP_HIPS_CLEANED_2$EQ5D_POSTOP_VAS<-ifelse(AMP_HIPS_CLEANED_2$EQVAS_POSTOP_TYPE ==
                                           "12MONTHS_ONLY", #condition
                                           AMP_HIPS_CLEANED_2$EQ5D_POSTOP12M_VAS, #then
                                           AMP_HIPS_CLEANED_2$EQ5D_POSTOP6M_VAS #else
) 
#.........................................................................................
#.........................................................................................



# Create AGEBAND var ------------------------------------------------------
###convert ages into age bands
AMP_HIPS_CLEANED_2$AGEBAND<-NA
AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGE <= 19,]$AGEBAND<-"0 to 19"
AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGE > 19 & AMP_HIPS_CLEANED_2$AGE <= 29,]$AGEBAND<-"20 to 29"
AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGE > 29 & AMP_HIPS_CLEANED_2$AGE <= 39,]$AGEBAND<-"30 to 39"
AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGE > 39 & AMP_HIPS_CLEANED_2$AGE <= 49,]$AGEBAND<-"40 to 49"
AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGE > 49 & AMP_HIPS_CLEANED_2$AGE <= 59,]$AGEBAND<-"50 to 59"
AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGE > 59 & AMP_HIPS_CLEANED_2$AGE <= 69,]$AGEBAND<-"60 to 69"
AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGE > 69 & AMP_HIPS_CLEANED_2$AGE <= 79,]$AGEBAND<-"70 to 79"
AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGE > 79 & AMP_HIPS_CLEANED_2$AGE <= 89,]$AGEBAND<-"80 to 89"
AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGE > 89,]$AGEBAND<-"90 to 120"

AMP_HIPS_CLEANED_2 %>% group_by(AGEBAND) %>% summarise(Min = min(AGE),
                                                     Max = max(AGE))
#table(AMP_HIPS_CLEANED_2$AGEBAND)

# Remove two cases with AGEBAND = "0 to 19" (this age-band is not included in English data)
AMP_HIPS_CLEANED_2<-AMP_HIPS_CLEANED_2[AMP_HIPS_CLEANED_2$AGEBAND != "0 to 19", ]
#nrow(AMP_HIPS_CLEANED_2) ## 823
#.........................................................................................
#.........................................................................................


# Convert OHS dimensions into numbers -------------------------------------
AMP_HIPS_CLEANED_3<-AMP_HIPS_CLEANED_2

##*01
## OHS_PREOP_PAIN
# 0 = Severe
# 1 = Moderate
# 2 = Mild
# 3 = Very mild
# 4 = None
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_PAIN) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_PAIN=="Severe",]$OHS_PREOP_PAIN<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_PAIN=="Moderate",]$OHS_PREOP_PAIN<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_PAIN=="Mild",]$OHS_PREOP_PAIN<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_PAIN=="Very mild",]$OHS_PREOP_PAIN<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_PAIN=="None",]$OHS_PREOP_PAIN<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_PAIN<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_PAIN)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_PAIN) %>% summarise(N = n())

##*02
## OHS_PREOP_SUDDENPAIN
# 0 = Every day
# 1 = Most days
# 2 = Some days
# 3 = Only 1 or 2 days
# 4 = No days
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_SUDDENPAIN) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SUDDENPAIN=="Every day",]$OHS_PREOP_SUDDENPAIN<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SUDDENPAIN=="Most days",]$OHS_PREOP_SUDDENPAIN<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SUDDENPAIN=="Some days",]$OHS_PREOP_SUDDENPAIN<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SUDDENPAIN=="Only 1 or 2 days",]$OHS_PREOP_SUDDENPAIN<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SUDDENPAIN=="No days",]$OHS_PREOP_SUDDENPAIN<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_SUDDENPAIN<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_SUDDENPAIN)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_SUDDENPAIN) %>% summarise(N = n())

##*03
## OHS_PREOP_NIGHTPAIN
# 0 = Every night
# 1 = Most nights
# 2 = Some nights
# 3 = Only 1 or 2 nights
# 4 = No nights
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_NIGHTPAIN) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_NIGHTPAIN=="Every night",]$OHS_PREOP_NIGHTPAIN<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_NIGHTPAIN=="Most nights",]$OHS_PREOP_NIGHTPAIN<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_NIGHTPAIN=="Some nights",]$OHS_PREOP_NIGHTPAIN<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_NIGHTPAIN=="Only 1 or 2 nights",]$OHS_PREOP_NIGHTPAIN<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_NIGHTPAIN=="No Nights",]$OHS_PREOP_NIGHTPAIN<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_NIGHTPAIN<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_NIGHTPAIN)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_NIGHTPAIN) %>% summarise(N = n())

##*04
## OHS_PREOP_WASHING
# 0 = Impossible to do
# 1 = Extreme difficulty
# 2 = Moderate trouble
# 3 = Very little trouble
# 4 = No trouble at all
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_WASHING) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WASHING=="Impossible to do",]$OHS_PREOP_WASHING<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WASHING=="Extreme difficulty ",]$OHS_PREOP_WASHING<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WASHING=="Moderate trouble",]$OHS_PREOP_WASHING<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WASHING=="Very little trouble",]$OHS_PREOP_WASHING<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WASHING=="No trouble at all",]$OHS_PREOP_WASHING<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_WASHING<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_WASHING)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_WASHING) %>% summarise(N = n())

##*05
## OHS_PREOP_TRANSPORT
# 0 = Impossible to do
# 1 = Extreme difficulty
# 2 = Moderate trouble
# 3 = Very little trouble
# 4 = No trouble at all
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_TRANSPORT) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_TRANSPORT=="Impossible to do",]$OHS_PREOP_TRANSPORT<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_TRANSPORT=="Extreme difficulty ",]$OHS_PREOP_TRANSPORT<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_TRANSPORT=="Moderate trouble",]$OHS_PREOP_TRANSPORT<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_TRANSPORT=="Very little trouble",]$OHS_PREOP_TRANSPORT<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_TRANSPORT=="No trouble at all",]$OHS_PREOP_TRANSPORT<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_TRANSPORT<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_TRANSPORT)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_TRANSPORT) %>% summarise(N = n())

##*06
## OHS_PREOP_DRESSING
# 0 = No, impossible
# 1 = With extreme difficulty
# 2 = With moderate difficulty
# 3 = With little difficulty
# 4 = Yes, easily
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_DRESSING) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_DRESSING=="No, impossible",]$OHS_PREOP_DRESSING<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_DRESSING=="With extreme difficulty",]$OHS_PREOP_DRESSING<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_DRESSING=="With moderate difficulty",]$OHS_PREOP_DRESSING<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_DRESSING=="With little difficulty",]$OHS_PREOP_DRESSING<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_DRESSING=="Yes, easily",]$OHS_PREOP_DRESSING<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_DRESSING<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_DRESSING)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_DRESSING) %>% summarise(N = n())

##*07
## OHS_PREOP_SHOPPING
# 0 = No, impossible
# 1 = With extreme difficulty
# 2 = With moderate difficulty
# 3 = With little difficulty
# 4 = Yes, easily
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_SHOPPING) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SHOPPING=="No, impossible",]$OHS_PREOP_SHOPPING<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SHOPPING=="With extreme difficulty",]$OHS_PREOP_SHOPPING<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SHOPPING=="With moderate difficulty",]$OHS_PREOP_SHOPPING<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SHOPPING=="With little difficulty",]$OHS_PREOP_SHOPPING<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_SHOPPING=="Yes, easily",]$OHS_PREOP_SHOPPING<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_SHOPPING<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_SHOPPING)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_SHOPPING) %>% summarise(N = n())

##*08
## OHS_PREOP_WALKING
# 0 = Not at all pain severe on walking
# 1 = Around the house only
# 2 = 5 15 minutes
# 3 = 16 30 minutes
# 4 = No pain/more than 30 minutes
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_WALKING) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WALKING=="Not at all/pain severe when walking",]$OHS_PREOP_WALKING<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WALKING=="Around the house only",]$OHS_PREOP_WALKING<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WALKING=="5 to 15 minutes",]$OHS_PREOP_WALKING<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WALKING=="16 to 30 minutes",]$OHS_PREOP_WALKING<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WALKING=="No pain/More than 30 minutes",]$OHS_PREOP_WALKING<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_WALKING<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_WALKING)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_WALKING) %>% summarise(N = n())

##*09
## OHS_PREOP_LIMPING
# 0 = All of the time
# 1 = Most of the time
# 2 = Often, not just at first
# 3 = Sometimes or just at first
# 4 = Rarely/Never
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_LIMPING) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_LIMPING=="All the time",]$OHS_PREOP_LIMPING<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_LIMPING=="Most of the time",]$OHS_PREOP_LIMPING<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_LIMPING=="Often, not just at first",]$OHS_PREOP_LIMPING<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_LIMPING=="Sometimes, or just at first ",]$OHS_PREOP_LIMPING<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_LIMPING=="Rarely / never",]$OHS_PREOP_LIMPING<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_LIMPING<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_LIMPING)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_LIMPING) %>% summarise(N = n())


##*10
## OHS_PREOP_STAIRS
# 0 = No, impossible
# 1 = With extreme difficulty
# 2 = With moderate difficulty
# 3 = With little difficulty
# 4 = Yes, easily
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_STAIRS) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STAIRS=="No, impossible",]$OHS_PREOP_STAIRS<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STAIRS=="With extreme difficulty",]$OHS_PREOP_STAIRS<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STAIRS=="With moderate difficulty",]$OHS_PREOP_STAIRS<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STAIRS=="With little difficulty",]$OHS_PREOP_STAIRS<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STAIRS=="Yes, easily",]$OHS_PREOP_STAIRS<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_STAIRS<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_STAIRS)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_STAIRS) %>% summarise(N = n())

##*11
## OHS_PREOP_STANDING
# 0 = Unbearable
# 1 = Very painful
# 2 = Moderately painful
# 3 = Slightly painful
# 4 = not at all painful
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_STANDING) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STANDING=="Unbearable",]$OHS_PREOP_STANDING<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STANDING=="Very painful",]$OHS_PREOP_STANDING<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STANDING=="Moderately painful",]$OHS_PREOP_STANDING<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STANDING=="Slightly painful",]$OHS_PREOP_STANDING<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_STANDING=="Not at all painful",]$OHS_PREOP_STANDING<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_STANDING<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_STANDING)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_STANDING) %>% summarise(N = n())


##*12
## OHS_PREOP_WORK
# 0 = Totally
# 1 = Greatly
# 2 = Moderately
# 3 = A little bit
# 4 = Not at all
#AMP_HIPS_CLEANED_2 %>% group_by(OHS_PREOP_WORK) %>% summarise(N = n())
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WORK=="Totally",]$OHS_PREOP_WORK<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WORK=="Greatly",]$OHS_PREOP_WORK<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WORK=="Moderately",]$OHS_PREOP_WORK<-2
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WORK=="A little bit",]$OHS_PREOP_WORK<-3
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_PREOP_WORK=="Not at all",]$OHS_PREOP_WORK<-4
AMP_HIPS_CLEANED_3$OHS_PREOP_WORK<-as.integer(AMP_HIPS_CLEANED_3$OHS_PREOP_WORK)
#AMP_HIPS_CLEANED_3 %>% group_by(OHS_PREOP_WORK) %>% summarise(N = n())
#.........................................................................................
#.........................................................................................


# Harmonise vars to English data ------------------------------------------
##REVISION FIELD
#table(AMP_HIPS_CLEANED_3$REVISION)
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$REVISION == 'primary',]$REVISION<-0
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$REVISION == 'revision',]$REVISION<-1
#table(AMP_HIPS_CLEANED_3$REVISION)

##SEX FIELD
#table(AMP_HIPS_CLEANED_3$SEX)
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$SEX == 'M',]$SEX<-1
AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$SEX == 'F',]$SEX<-2
#table(AMP_HIPS_CLEANED_3$SEX)
#.........................................................................................
#.........................................................................................


# Creating outcome variables MCID -----------------------------------------
##Create OHS & EQ-VAS MCID


AMP_HIPS_CLEANED_3 <- as.data.frame(
  AMP_HIPS_CLEANED_3 %>% 
    mutate(OHS_TOTSCORE.diff = OHS_POSTOP_TOTSCORE - OHS_PREOP_TOTSCORE) %>%
    mutate(VAS_TOTSCORE.diff = EQ5D_POSTOP_VAS - EQ5D_PREOP_VAS) %>%
    # threshold value from literature 
    # (postOHS - preOHS >= 8 points of increase)
    mutate(OHS_MCID = ifelse(OHS_TOTSCORE.diff>=8, 1,0))%>%
    # threshold value from pre-op EQ-VAS distributions
    # (postvas - preVAS >= 11 points of increase ---> 11 is 0.5*std_dev(preVAS))
    # std_dev(preVAS) --> this value has been calculated for English Training set
    mutate(VAS_MCID = ifelse(VAS_TOTSCORE.diff>=11, 1,0)))
#.........................................................................................
#.........................................................................................


# Vars factor transformation ----------------------------------------------
#### Transforming relevant variables into factors  (cannot do this directly with outcomes!) 
AMP_HIPS_CLEANED_3[,c("REVISION","AGEBAND","SEX")] <-
  lapply(AMP_HIPS_CLEANED_3[,c("REVISION","AGEBAND","SEX")], as.factor) 
#.........................................................................................
#.........................................................................................

# Reduce sample size for OHS ----------------------------------------------
AMP_HIPS_CLEANED_3.small<-AMP_HIPS_CLEANED_3[AMP_HIPS_CLEANED_3$OHS_POSTOP_TOTSCORE_TYPE!="12MONTHS_ONLY" ,]
#nrow(AMP_HIPS_CLEANED_3.small) #440
#.........................................................................................
#.........................................................................................

# Extract demographics ----------------------------------------------------
DescriptiveCatBasic(input_dataset = AMP_HIPS_CLEANED_3, 
                    output_table = Descr_Categ.AMP_HIPS_CLEANED_3)
DescriptiveCont2(input_dataset = AMP_HIPS_CLEANED_3, 
                 output_table = Descr_Continuous.AMP_HIPS_CLEANED_3)

write.csv(Descr_Categ.AMP_HIPS_CLEANED_3,"output/thesis_files/Descr_Categ.AMP_HIPS_CLEANED_3.csv", row.names = FALSE)
write.csv(Descr_Continuous.AMP_HIPS_CLEANED_3,"output/thesis_files/Descr_Continuous.AMP_HIPS_CLEANED_3.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................



# Plots -------------------------------------------------------------------
#### Plotting distributions for differences (PostOp - PreOp) in OHS and EQ-VAS
#### including MCID thresholding for Welsh dataset

## Histogram of difference [OHS PostOp - PreOp]
Hist_OHS_test.AMPLITUDE<-ggplot(AMP_HIPS_CLEANED_3.small, aes(x = OHS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("PostOperative OHS change\n(Welsh Test Set)")+
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
  scale_x_continuous(limits=c(-48,48),breaks = seq(-48,48,4)) +
  geom_vline(xintercept=8, colour="red", linetype = "longdash", size = 1)  +
  annotate(x=0,y=+Inf,label="MCID\nthreshold",vjust=2,geom="text", colour="red", size = 4)

## Histogram of difference [EQ-VAS PostOp - PreOp]
Hist_VAS_test.AMPLITUDE<-ggplot(AMP_HIPS_CLEANED_3, aes(x = VAS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("PostOperative EQ-VAS change\n(Welsh Test Set)")+
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
  scale_x_continuous(limits=c(-100,100),breaks=seq(-100,100,10)) +
  geom_vline(xintercept=11, colour="red", linetype = "longdash", size = 1)  + #check the threshold line is plotted at std dev*0.5
  annotate(x=30,y=+Inf,label="MCID\nthreshold",vjust=2,geom="text", colour="red", size = 4)

## Histogram of preOp OHS
Hist_preopOHS_AMP_HIPS_CLEANED3<-ggplot(AMP_HIPS_CLEANED_3.small, aes(x = OHS_PREOP_TOTSCORE)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PreOperative OHS tot score\n(Welsh Test Set)")+
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
Hist_preopVAS_AMP_HIPS_CLEANED3<-ggplot(AMP_HIPS_CLEANED_3, aes(x = EQ5D_PREOP_VAS)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PreOperative EQ-VAS score\n(Welsh Test Set)")+
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



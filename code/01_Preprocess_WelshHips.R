###### PRE-PROCESSING - Welsh Hips dataset (PRIMARY & REVISION)

### Import libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
source("code/00_ProjectFunctions.R") # library of custom-made functions
#.........................................................................................

### Import input data -------------------------------------------------------
AMP_HIPS_PRIM<-read.csv("input_data/Welsh_data/Amplitude_HipsPrimary190404_forAuraFrizzatiMSc_anonRP210414.csv", 
                        header = T,
                        na.strings=c("", "I", "DUE", "OVERDUE", "NA"))

AMP_HIPS_REV<-read.csv("input_data/Welsh_data/Amplitude_HipsRevision170803_forAuraFrizzatiMSc_anonRP210414.csv", 
                       header = T,
                       na.strings=c("", "I", "DUE", "OVERDUE", "NA"))
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
sapply(AMP_HIPS_PRIM.cleaned3, function(x){sum(is.na(x))})

## remove 1 subject without all pre-op OHS dimensions:
AMP_HIPS_PRIM.cleaned3<-AMP_HIPS_PRIM.cleaned3[!is.na(AMP_HIPS_PRIM.cleaned3$OHS_PREOP_SHOPPING),]

##checking N of missing values/column
sapply(AMP_HIPS_PRIM.cleaned3, function(x){sum(is.na(x))})

## After these cleaning steps, only these predictors have missing values:
## OHS_POSTOP6M_TOTSCORE, OHS_POSTOP12M_TOTSCORE, EQ5D_POSTOP6M_VAS,  EQ5D_POSTOP12M_VAS

rm(AMP_HIPS_PRIM.cleaned2)
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





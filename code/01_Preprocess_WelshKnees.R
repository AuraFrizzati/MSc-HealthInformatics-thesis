###### PRE-PROCESSING - Welsh Knees dataset (PRIMARY & REVISION)

### Import libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
source("code/00_ProjectFunctions.R") # library of custom-made functions
#.........................................................................................
#.........................................................................................

### Import input data -------------------------------------------------------
AMP_KNEES_PRIM<-read.csv("input_data/Welsh_data/Amplitude_KneesPrimary170803_forAuraFrizzatiMSc_anonRP210414.csv", 
                         header = T, 
                         na.strings=c("", "I", "DUE", "OVERDUE", "NA")
)

AMP_KNEES_REV<-read.csv("input_data/Welsh_data/Amplitude_KneesRevision170803_forAuraFrizzatiMSc_anonRP210414.csv", 
                        header = T, 
                        na.strings=c("", "I", "DUE", "OVERDUE", "NA")
)
#.........................................................................................
#.........................................................................................

# Variables' format &  missing values -------------------------------------
## PRIMARY UNIQUE VALUES
values_by_column(input_dataset="AMP_KNEES_PRIM", 
                 output_dir="output/extra_files/", 
                 output_name="AMP_KNEES_PRIM")
# VALUES FOR REVISIONS SEEM ALL OK (I already have age and I do not need to convert dates)
## TO FLAG AS MISSING VALUES for PRIMARY: 
# - GENDER: "I"
# - VARIOUS DATES: "DUE", "OVERDUE"
# - "NA" as a string variable
## REVISIONS UNIQUE VALUES
values_by_column(input_dataset="AMP_KNEES_REV", 
                 output_dir="output/extra_files/", 
                 output_name="AMP_KNEES_REV")
# VALUES FOR REVISIONS SEEM ALL OK (I already have age and I do not need to convert dates)
#.........................................................................................
#.........................................................................................

# Primary: patients 2+ primary surgeries on the same side--------
### check for the presence of patients with multiple primary surgeries on the same side
AMP_KNEES_PRIM %>% 
  group_by(pID,Pathway.Side) %>% 
  summarise (N_Primary_sameSide = n()) %>%
  arrange(-N_Primary_sameSide)
AMP_KNEES_PRIM %>% 
  group_by(pID,Pathway.Side) %>% 
  summarise (N_Primary_sameSide = n()) %>%
  group_by(N_Primary_sameSide) %>%
  summarise (Tot = n())
## Extract list of subjects with N_Primary_sameSide > 1
remove<-as.data.frame(AMP_KNEES_PRIM %>% 
                        group_by(pID,Pathway.Side) %>% 
                        summarise (N_Primary_sameSide = n()) %>% 
                        filter(N_Primary_sameSide > 1)%>% 
                        unite(excl_filter, c("pID", "Pathway.Side"), sep = ""))
### remove subjects with N_Primary_sameSide > 1 
AMP_KNEES_PRIM$combID<-paste0(AMP_KNEES_PRIM$pID, AMP_KNEES_PRIM$Pathway.Side)
AMP_KNEES_PRIM.cleaned<-AMP_KNEES_PRIM[!(AMP_KNEES_PRIM$combID %in% remove$excl_filter),]
rm(remove)
#.........................................................................................
#.........................................................................................

# Primary: re-organise columns ------------------------------------------------------
### drop  unnecessary columns
drop_columns<-c("Pathway.Side",
                "Activity.Date",
                "Start.Date1",
                "Completed.Date...Oxford.Knee.Score...Score...Baseline",
                "Completed.Date...Oxford.Knee.Score...Score...6.Months",
                "Completed.Date...Oxford.Knee.Score...Score...12.Months",
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
AMP_KNEES_PRIM.cleaned2<-
  AMP_KNEES_PRIM.cleaned[ , 
                          !(names(AMP_KNEES_PRIM.cleaned) %in% drop_columns)]# from 42 to 23 variables
## re-name columns 
new_col_names<-c("pID",
                 "REVISION",
                 "SEX",
                 "AGE",
                 "OKS_PREOP_TOTSCORE",
                 "OKS_POSTOP6M_TOTSCORE",
                 "OKS_POSTOP12M_TOTSCORE",
                 "EQ5D_PREOP_VAS",
                 "EQ5D_POSTOP6M_VAS",
                 "EQ5D_POSTOP12M_VAS",
                 "EQ5D_PREOP_INDEX",
                 "OKS_PREOP_SHOPPING",
                 "OKS_PREOP_KNEELING",
                 "OKS_PREOP_STAIRS",
                 "OKS_PREOP_STANDING",
                 "OKS_PREOP_WALKING",
                 "OKS_PREOP_LIMPING",
                 "OKS_PREOP_NIGHT_PAIN",
                 "OKS_PREOP_CONFIDENCE",
                 "OKS_PREOP_TRANSPORT",
                 "OKS_PREOP_WASHING",
                 "OKS_PREOP_WORK",
                 "OKS_PREOP_PAIN")
# extract df columns' original names and add new columns' names 
colnames_orig_and_new <- cbind(as.data.frame(colnames(AMP_KNEES_PRIM.cleaned2)), new_col_names)
# rename the columns of the original df with the new names:
names(AMP_KNEES_PRIM.cleaned2)[match(colnames_orig_and_new[,1], 
                                     names(AMP_KNEES_PRIM.cleaned2))] = colnames_orig_and_new[,2]
rm(drop_columns,new_col_names,colnames_orig_and_new, AMP_KNEES_PRIM.cleaned)
#.........................................................................................
#.........................................................................................

# Primary: missing values baseline OKS and EQ-VAS ----------------------------------
## check N of missing values for the baseline OKS and EQ-VAS scores
AMP_KNEES_PRIM.cleaned2$BASELINE_SCORE_TYPE<-NA
AMP_KNEES_PRIM.cleaned2[!is.na(AMP_KNEES_PRIM.cleaned2$OKS_PREOP_TOTSCORE)&
                          is.na(AMP_KNEES_PRIM.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"OKSPREOP_ONLY"
AMP_KNEES_PRIM.cleaned2[!is.na(AMP_KNEES_PRIM.cleaned2$OKS_PREOP_TOTSCORE)&
                          !is.na(AMP_KNEES_PRIM.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"OKSPREOP&VASPREOP"
AMP_KNEES_PRIM.cleaned2[is.na(AMP_KNEES_PRIM.cleaned2$OKS_PREOP_TOTSCORE)&
                          !is.na(AMP_KNEES_PRIM.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"VASPREOP_ONLY"
AMP_KNEES_PRIM.cleaned2 %>% group_by(BASELINE_SCORE_TYPE) %>% summarise (tot = n())
## remove subjects without both OKS and VAS pre-op:
AMP_KNEES_PRIM.cleaned3<-AMP_KNEES_PRIM.cleaned2[AMP_KNEES_PRIM.cleaned2$BASELINE_SCORE_TYPE=="OKSPREOP&VASPREOP"&
                                                   !is.na(AMP_KNEES_PRIM.cleaned2$BASELINE_SCORE_TYPE),]
##checking N of missing values/column
#sapply(AMP_KNEES_PRIM.cleaned3, function(x){sum(is.na(x))})
## After these cleaning steps, only these predictors have missing values:
## OKS_POSTOP6M_TOTSCORE, OKS_POSTOP12M_TOTSCORE, EQ5D_POSTOP6M_VAS,  EQ5D_POSTOP12M_VAS
rm(AMP_KNEES_PRIM.cleaned2)
#.........................................................................................
#.........................................................................................

# Primary: remove cases without tot OKS 6 or 12 months --------------------
## check combinations of OKS TOT SCORE 6-months and 12-months
AMP_KNEES_PRIM.cleaned3$OKS_POSTOP_TOTSCORE_TYPE<-NA
AMP_KNEES_PRIM.cleaned3[!is.na(AMP_KNEES_PRIM.cleaned3$OKS_POSTOP6M_TOTSCORE)&
                          is.na(AMP_KNEES_PRIM.cleaned3$OKS_POSTOP12M_TOTSCORE),]$OKS_POSTOP_TOTSCORE_TYPE<-"6MONTHS_ONLY"
AMP_KNEES_PRIM.cleaned3[!is.na(AMP_KNEES_PRIM.cleaned3$OKS_POSTOP6M_TOTSCORE)&
                          !is.na(AMP_KNEES_PRIM.cleaned3$OKS_POSTOP12M_TOTSCORE),]$OKS_POSTOP_TOTSCORE_TYPE<-"6MONTHS&12MONTHS"
AMP_KNEES_PRIM.cleaned3[is.na(AMP_KNEES_PRIM.cleaned3$OKS_POSTOP6M_TOTSCORE)&
                          !is.na(AMP_KNEES_PRIM.cleaned3$OKS_POSTOP12M_TOTSCORE),]$OKS_POSTOP_TOTSCORE_TYPE<-"12MONTHS_ONLY"
AMP_KNEES_PRIM.cleaned3 %>% group_by(OKS_POSTOP_TOTSCORE_TYPE) %>% summarise (tot = n())
### remove rows without OHS 6-month post-op & without OHS 12-month post-op
AMP_KNEES_PRIM.cleaned4<-AMP_KNEES_PRIM.cleaned3[!is.na(AMP_KNEES_PRIM.cleaned3$OKS_POSTOP_TOTSCORE_TYPE),]
rm(AMP_KNEES_PRIM.cleaned3)
#.........................................................................................
#.........................................................................................

# Primary: remove cases without EQVAS 6 or 12 months --------------------
## check combinations of EQ-VAS 6-months and 12-months
AMP_KNEES_PRIM.cleaned4$EQVAS_POSTOP_TYPE<-NA
AMP_KNEES_PRIM.cleaned4[!is.na(AMP_KNEES_PRIM.cleaned4$EQ5D_POSTOP6M_VAS)&
                          is.na(AMP_KNEES_PRIM.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"6MONTHS_ONLY"
AMP_KNEES_PRIM.cleaned4[!is.na(AMP_KNEES_PRIM.cleaned4$EQ5D_POSTOP6M_VAS)&
                          !is.na(AMP_KNEES_PRIM.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"6MONTHS&12MONTHS"
AMP_KNEES_PRIM.cleaned4[is.na(AMP_KNEES_PRIM.cleaned4$EQ5D_POSTOP6M_VAS)&
                          !is.na(AMP_KNEES_PRIM.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"12MONTHS_ONLY"
AMP_KNEES_PRIM.cleaned4 %>% group_by(EQVAS_POSTOP_TYPE) %>% summarise (tot = n())
### remove rows without EQ-VAS 6-month post-op & without EQ-VAS 12-month post-op
AMP_KNEES_PRIM.cleaned5<-AMP_KNEES_PRIM.cleaned4[!is.na(AMP_KNEES_PRIM.cleaned4$EQVAS_POSTOP_TYPE),]
rm(AMP_KNEES_PRIM.cleaned4)
#.........................................................................................
#.........................................................................................

# Revision: patients 2+ primary surgeries on the same side--------
### check for the presence of patients with multiple revision surgeries on the same side  (although these might be plausible)
AMP_KNEES_REV %>% 
  group_by(pID,Pathway.Side) %>% 
  summarise (N_Revision_sameSide = n()) %>%
  group_by(N_Revision_sameSide) %>%
  summarise (Tot = n())
AMP_KNEES_REV.cleaned<-AMP_KNEES_REV #just to recycle names below...
#.........................................................................................
#.........................................................................................

# Revision: re-organise columns ------------------------------------------------------
### drop  unnecessary columns
drop_columns<-c("Pathway.Side",
                "Activity.Date",
                "Start.Date1",
                "Completed.Date...Oxford.Knee.Score...Score...Baseline",
                "Completed.Date...Oxford.Knee.Score...Score...6.Months",
                "Completed.Date...Oxford.Knee.Score...Score...12.Months",
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
AMP_KNEES_REV.cleaned2<-
  AMP_KNEES_REV.cleaned[ , 
                         !(names(AMP_KNEES_REV.cleaned) %in% drop_columns)]
# from 42 to 23 variables
## re-name columns 
new_col_names<-c("pID",
                 "REVISION",
                 "SEX",
                 "AGE",
                 "OKS_PREOP_TOTSCORE",
                 "OKS_POSTOP6M_TOTSCORE",
                 "OKS_POSTOP12M_TOTSCORE",
                 "EQ5D_PREOP_VAS",
                 "EQ5D_POSTOP6M_VAS",
                 "EQ5D_POSTOP12M_VAS",
                 "EQ5D_PREOP_INDEX",
                 "OKS_PREOP_SHOPPING",
                 "OKS_PREOP_KNEELING",
                 "OKS_PREOP_STAIRS",
                 "OKS_PREOP_STANDING",
                 "OKS_PREOP_WALKING",
                 "OKS_PREOP_LIMPING",
                 "OKS_PREOP_NIGHT_PAIN",
                 "OKS_PREOP_CONFIDENCE",
                 "OKS_PREOP_TRANSPORT",
                 "OKS_PREOP_WASHING",
                 "OKS_PREOP_WORK",
                 "OKS_PREOP_PAIN")
# extract df columns' original names and add new columns' names 
colnames_orig_and_new <- cbind(as.data.frame(colnames(AMP_KNEES_REV.cleaned2)), new_col_names)
# rename the columns of the original df with the new names:
names(AMP_KNEES_REV.cleaned2)[match(colnames_orig_and_new[,1], 
                                    names(AMP_KNEES_REV.cleaned2))] = colnames_orig_and_new[,2]
rm(drop_columns,new_col_names,colnames_orig_and_new, AMP_KNEES_REV.cleaned)
#.........................................................................................
#.........................................................................................

# Revision: missing values baseline OKS and EQ-VAS ----------------------------------
## check N of missing values for the baseline OKS and EQ-VAS scores
AMP_KNEES_REV.cleaned2$BASELINE_SCORE_TYPE<-NA
AMP_KNEES_REV.cleaned2[!is.na(AMP_KNEES_REV.cleaned2$OKS_PREOP_TOTSCORE)&
                         is.na(AMP_KNEES_REV.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"OKSPREOP_ONLY"
AMP_KNEES_REV.cleaned2[!is.na(AMP_KNEES_REV.cleaned2$OKS_PREOP_TOTSCORE)&
                         !is.na(AMP_KNEES_REV.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"OKSPREOP&VASPREOP"
AMP_KNEES_REV.cleaned2[is.na(AMP_KNEES_REV.cleaned2$OKS_PREOP_TOTSCORE)&
                         !is.na(AMP_KNEES_REV.cleaned2$EQ5D_PREOP_VAS),]$BASELINE_SCORE_TYPE<-"VASPREOP_ONLY"
AMP_KNEES_REV.cleaned2 %>% group_by(BASELINE_SCORE_TYPE) %>% summarise (tot = n())
## remove subjects without both OKS and VAS pre-op:
AMP_KNEES_REV.cleaned3<-AMP_KNEES_REV.cleaned2[AMP_KNEES_REV.cleaned2$BASELINE_SCORE_TYPE=="OKSPREOP&VASPREOP"&
                                                 !is.na(AMP_KNEES_REV.cleaned2$BASELINE_SCORE_TYPE),]
##checking N of missing values/column
#sapply(AMP_KNEES_REV.cleaned3, function(x){sum(is.na(x))})
rm(AMP_KNEES_REV.cleaned2)
## After these cleaning steps, only these predictors have missing values:
## OKS_POSTOP6M_TOTSCORE, OKS_POSTOP12M_TOTSCORE, EQ5D_POSTOP6M_VAS,  EQ5D_POSTOP12M_VAS
#.........................................................................................
#.........................................................................................

# Revision: remove cases without tot OKS 6 or 12 months --------------------
## check combinations of OKS TOT SCORE 6-months and 12-months
AMP_KNEES_REV.cleaned3$OKS_POSTOP_TOTSCORE_TYPE<-NA
AMP_KNEES_REV.cleaned3[!is.na(AMP_KNEES_REV.cleaned3$OKS_POSTOP6M_TOTSCORE)&
                         is.na(AMP_KNEES_REV.cleaned3$OKS_POSTOP12M_TOTSCORE),]$OKS_POSTOP_TOTSCORE_TYPE<-"6MONTHS_ONLY"
AMP_KNEES_REV.cleaned3[!is.na(AMP_KNEES_REV.cleaned3$OKS_POSTOP6M_TOTSCORE)&
                         !is.na(AMP_KNEES_REV.cleaned3$OKS_POSTOP12M_TOTSCORE),]$OKS_POSTOP_TOTSCORE_TYPE<-"6MONTHS&12MONTHS"
AMP_KNEES_REV.cleaned3[is.na(AMP_KNEES_REV.cleaned3$OKS_POSTOP6M_TOTSCORE)&
                         !is.na(AMP_KNEES_REV.cleaned3$OKS_POSTOP12M_TOTSCORE),]$OKS_POSTOP_TOTSCORE_TYPE<-"12MONTHS_ONLY"
AMP_KNEES_REV.cleaned3 %>% group_by(OKS_POSTOP_TOTSCORE_TYPE) %>% summarise (tot = n())
### remove rows without OKS 6-month post-op & without OKS 12-month post-op
AMP_KNEES_REV.cleaned4<-AMP_KNEES_REV.cleaned3[!is.na(AMP_KNEES_REV.cleaned3$OKS_POSTOP_TOTSCORE_TYPE),]
rm(AMP_KNEES_REV.cleaned3)
#.........................................................................................
#.........................................................................................

# Revision: remove cases without EQVAS 6 or 12 months --------------------
## check combinations of EQ-VAS 6-months and 12-months
AMP_KNEES_REV.cleaned4$EQVAS_POSTOP_TYPE<-NA
AMP_KNEES_REV.cleaned4[!is.na(AMP_KNEES_REV.cleaned4$EQ5D_POSTOP6M_VAS)&
                         is.na(AMP_KNEES_REV.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"6MONTHS_ONLY"
AMP_KNEES_REV.cleaned4[!is.na(AMP_KNEES_REV.cleaned4$EQ5D_POSTOP6M_VAS)&
                         !is.na(AMP_KNEES_REV.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"6MONTHS&12MONTHS"
AMP_KNEES_REV.cleaned4[is.na(AMP_KNEES_REV.cleaned4$EQ5D_POSTOP6M_VAS)&
                         !is.na(AMP_KNEES_REV.cleaned4$EQ5D_POSTOP12M_VAS),]$EQVAS_POSTOP_TYPE<-"12MONTHS_ONLY"
AMP_KNEES_REV.cleaned4 %>% group_by(EQVAS_POSTOP_TYPE) %>% summarise (tot = n())
### remove rows without EQ-VAS 6-month post-op & without EQ-VAS 12-month post-op
AMP_KNEES_REV.cleaned5<-AMP_KNEES_REV.cleaned4[!is.na(AMP_KNEES_REV.cleaned4$EQVAS_POSTOP_TYPE),]
rm(AMP_KNEES_REV.cleaned4)
#.........................................................................................
#.........................................................................................

# Merge Primary & Revision ------------------------------------------------
## check the two datasets' columns
#names(AMP_KNEES_PRIM.cleaned5)
#names(AMP_KNEES_REV.cleaned5)
##vertically merge two datasets' columns
AMP_KNEES_CLEANED<-rbind(AMP_KNEES_PRIM.cleaned5,AMP_KNEES_REV.cleaned5)
### Compare distributions of post-op OKS at 6 and 12 months
#extract 6 months
OKS_POSTOP6M_TOTSCORE.p<-as.data.frame(AMP_KNEES_CLEANED$OKS_POSTOP6M_TOTSCORE)
OKS_POSTOP6M_TOTSCORE.p$postTime<-'6M'
names(OKS_POSTOP6M_TOTSCORE.p)<-c('OKS','postTime')
OKS_POSTOP6M_TOTSCORE.p<-OKS_POSTOP6M_TOTSCORE.p[!is.na(OKS_POSTOP6M_TOTSCORE.p$OKS),] ##309
#extract 12 months
OKS_POSTOP12M_TOTSCORE.p<-as.data.frame(AMP_KNEES_CLEANED$OKS_POSTOP12M_TOTSCORE)
OKS_POSTOP12M_TOTSCORE.p$postTime<-'12M'
names(OKS_POSTOP12M_TOTSCORE.p)<-c('OKS','postTime')
OKS_POSTOP12M_TOTSCORE.p<-OKS_POSTOP12M_TOTSCORE.p[!is.na(OKS_POSTOP12M_TOTSCORE.p$OKS),] ##165
#bind
OKS_POSTOP_TOTSCORE.p<-rbind(OKS_POSTOP6M_TOTSCORE.p,OKS_POSTOP12M_TOTSCORE.p)
rm(OKS_POSTOP6M_TOTSCORE.p,OKS_POSTOP12M_TOTSCORE.p)
OKS_POSTOP_TOTSCORE.p$postTime<-factor(OKS_POSTOP_TOTSCORE.p$postTime,
                                       levels = c("6M", "12M"))
## visually checking OKS distributions (post op 6 vs 12 months):
ggplot() + 
  geom_histogram(aes(x=OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=='6M',]$OKS, 
                     fill = OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=='6M',]$postTime), 
                 alpha = 0.5) +
  geom_histogram(aes(x=OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=='12M',]$OKS, 
                     fill = OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=='12M',]$postTime),
                 alpha = 0.5)+
  xlab("post-op OKS tot score") + ylab("Number of subjects") +
  ggtitle("Amplitude Knees - Primary+Revision dataset")+
  scale_fill_manual(name="post-op OKS variables", 
                    values=c("red","green"),
                    labels=c("6 months", "12 months"))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.9, 0.9))
boxplot_prim_vs_rev_OKS <- ggplot(OKS_POSTOP_TOTSCORE.p, aes(x=postTime, y=OKS)) + 
  geom_boxplot(aes(fill=factor(postTime))) + 
  geom_point(size=0.5) +   
  #stat_summary(geom = "errorbar", fun.min = mean, fun = mean, fun.max = mean, 
  #             width = .75, color = "red")+ ## add mean to box plots
  xlab("Collection time of post-operative OKS questionnaire") + 
  ylab("Post-operative OKS total score") +
  ggtitle("Welsh Knee Dataset - OKS")+ ##primary + revision
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c("6 months post-surgery","12 months post-surgery")) +
  scale_fill_manual(values=c("darkgrey","white"))+ 
  theme(legend.position = "none")+ ylim(0,55)+
  scale_y_continuous(breaks = seq(0,50, by = 10))
## print image
png(filename="output/thesis_files/boxplot_prim_vs_rev_OKS.png",  width = 473, height = 363)
boxplot_prim_vs_rev_OHS
dev.off()
OKS_POSTOP_TOTSCORE.p %>% group_by(postTime) %>% summarise (Mean = mean(OKS),
                                                            StdDev = sd(OKS),
                                                            Min = min(OKS),
                                                            Max = max(OKS),
                                                            Median = median(OKS),
                                                            FirstQuartile = quantile(OKS, probs = 0.25),
                                                            ThirdQuartile = quantile(OKS, probs = 0.75))
g1<-ggplot(OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=="6M",], 
           aes(sample=OKS))+
  stat_qq(shape=1) + 
  stat_qq_line(fullrange = FALSE) +
  ggtitle("QQ-plot 6-month post-operative\nOKS tot scores (Welsh Knee Dataset)")+
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
g2<-ggplot(OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=="12M",], 
           aes(sample=OKS))+
  stat_qq(shape=1) + 
  stat_qq_line(fullrange = FALSE) +
  ggtitle("QQ-plot 12-month post-operative\nOKS tot scores (Welsh Knee Dataset)")+
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
g3<-ggplot(OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=="6M",], 
           aes(x = OKS)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("6-month post-operative OKS tot scores\n(Welsh Knee Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("6-month post-operative OKS total score")+
  scale_x_continuous(limits=c(0,48),breaks = seq(0,48,4))+
  geom_vline(xintercept=mean(OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=="6M",]$OKS), colour="red", linetype = "longdash", size = 1)+
  geom_vline(xintercept=median(OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=="6M",]$OKS), colour="black", size = 1)
g4<-ggplot(OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=="12M",], 
           aes(x = OKS)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("12-month post-operative OKS tot scores\n(Welsh Knee Dataset)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("12-month post-operative OKS total score")+
  scale_x_continuous(limits=c(0,48),breaks = seq(0,48,4))+
  geom_vline(xintercept=mean(OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=="12M",]$OKS), colour="red", linetype = "longdash", size = 1)+
  geom_vline(xintercept=median(OKS_POSTOP_TOTSCORE.p[OKS_POSTOP_TOTSCORE.p$postTime=="12M",]$OKS), colour="black", size = 1)
ggpubr::ggarrange(g1,g3,g2,g4)
### check distributions' normality
with(OKS_POSTOP_TOTSCORE.p, shapiro.test(OKS[postTime == "6M"]))
# W = 0.94694, p-value = 4.146e-09 --> NOT NORMALLY DISTRIBUTED
with(OKS_POSTOP_TOTSCORE.p, shapiro.test(OKS[postTime == "12M"]))
# W = 0.92467, p-value = 1.431e-07 --> NOT NORMALLY DISTRIBUTED
## non parametric test: Wilcoxon rank sum (or Mann-Whitney) test:
wilcox.test(OKS ~ postTime, data = OKS_POSTOP_TOTSCORE.p, exact = FALSE)
# Wilcoxon rank sum test with continuity correction
# data:  OKS by postTime
# W = 25734, p-value = 0.8652
# alternative hypothesis: true location shift is not equal to 0
AMP_KNEES_CLEANED %>% group_by(OKS_POSTOP_TOTSCORE_TYPE) %>% summarise (tot = n())
rm(g1,g3,g2,g4,OKS_POSTOP_TOTSCORE.p)
#.........................................................................................
#.........................................................................................

# Compare postop EQVAS 6 vs 12 months -----------------------------------
#### Compare distributions of post-op EQ-VAS at 6 and 12 months
#extract 6 months
EQVAS_POSTOP6M.p<-as.data.frame(AMP_KNEES_CLEANED$EQ5D_POSTOP6M_VAS)
EQVAS_POSTOP6M.p$postTime<-'6M'
names(EQVAS_POSTOP6M.p)<-c('EQVAS','postTime')
EQVAS_POSTOP6M.p<-EQVAS_POSTOP6M.p[!is.na(EQVAS_POSTOP6M.p$EQVAS),] ##293
#extract 12 months
EQVAS_POSTOP12M.p<-as.data.frame(AMP_KNEES_CLEANED$EQ5D_POSTOP12M_VAS)
EQVAS_POSTOP12M.p$postTime<-'12M'
names(EQVAS_POSTOP12M.p)<-c('EQVAS','postTime')
EQVAS_POSTOP12M.p<-EQVAS_POSTOP12M.p[!is.na(EQVAS_POSTOP12M.p$EQVAS),] ##161
#bind
EQVAS_POSTOP.p<-rbind(EQVAS_POSTOP6M.p,EQVAS_POSTOP12M.p)
EQVAS_POSTOP.p$postTime<-factor(EQVAS_POSTOP.p$postTime,
                                levels = c("6M", "12M"))
## visually checking EQVAS distributions (post op 6 vs 12 months):
ggplot() + 
  geom_histogram(aes(x=EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=='6M',]$EQVAS, 
                     fill = EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=='6M',]$postTime), 
                 alpha = 0.5) +
  geom_histogram(aes(x=EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=='12M',]$EQVAS, 
                     fill = EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=='12M',]$postTime),
                 alpha = 0.5)+
  xlab("post-op EQ-VAS tot score") + ylab("Number of subjects") +
  ggtitle("Amplitude Knees - Primary+Revision dataset")+
  scale_fill_manual(name="post-op EQ-VAS variables", 
                    values=c("red","green"),
                    labels=c("6 months", "12 months"))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = c(0.9, 0.9))
boxplot_prim_vs_rev_EQVAS_knees<-ggplot(EQVAS_POSTOP.p, aes(x=postTime, y=EQVAS)) + 
  geom_boxplot(aes(fill=factor(postTime))) + 
  geom_point(size=0.5) +   
  #stat_summary(geom = "errorbar", fun.min = mean, fun = mean, fun.max = mean, 
  #             width = .75, color = "red")+ ## add mean to box plots
  xlab("Collection time of post-operative EQ-VAS questionnaire") + 
  ylab("Post-operative EQ-VAS score") +
  ggtitle("Welsh Knee Dataset - EQ-VAS")+ ##primary + revision 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c("6 months post-surgery","12 months post-surgery")) +
  scale_fill_manual(values=c("darkgrey","white"))+ 
  theme(legend.position = "none")+ ylim(0,55)+
  scale_y_continuous(breaks = seq(0,110, by = 10))
## print image
png(filename="output/thesis_files/boxplot_prim_vs_rev_EQVAS_knees.png",  width = 473, height = 363)
boxplot_prim_vs_rev_EQVAS_knees
dev.off()
EQVAS_POSTOP.p %>% group_by(postTime) %>% summarise (Mean = mean(EQVAS),
                                                     StdDev = sd(EQVAS),
                                                     Min = min(EQVAS),
                                                     Max = max(EQVAS),
                                                     Median = median(EQVAS),
                                                     FirstQuartile = quantile(EQVAS, probs = 0.25),
                                                     ThirdQuartile = quantile(EQVAS, probs = 0.75))
g5<-ggplot(EQVAS_POSTOP.p[EQVAS_POSTOP.p$postTime=="6M",], 
           aes(sample=EQVAS))+
  stat_qq(shape=1) + 
  stat_qq_line(fullrange = FALSE) +
  ggtitle("QQ-plot 6-month post-operative\nEQ-VAS tot scores (Welsh Knee Dataset)")+
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
  ggtitle("QQ-plot 12-month post-operative\nEQ-VAS tot scores (Welsh Knee Dataset)")+
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
  ggtitle("6-month post-operative EQ-VAS tot scores\n(Welsh Knee Dataset)")+
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
  ggtitle("12-month post-operative EQ-VAS tot scores\n(Welsh Knee Dataset)")+
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
## check distributions' normality
with(EQVAS_POSTOP.p, shapiro.test(EQVAS[postTime == "6M"]))
# W = 0.92891, p-value = 1.283e-10 --> NOT NORMALLY DISTRIBUTED
with(EQVAS_POSTOP.p, shapiro.test(EQVAS[postTime == "12M"]))
# W = 0.93949, p-value = 2.334e-06 --> NOT NORMALLY DISTRIBUTED
## non parametric test: Wilcoxon rank sum (or Mann-Whitney) test:
wilcox.test(EQVAS ~ postTime, data = EQVAS_POSTOP.p, exact = FALSE)
# Wilcoxon rank sum test with continuity correction
# data:  OKS by postTime
# W = 26682, p-value = 0.02038
# alternative hypothesis: true location shift is not equal to 0
## create filter to use for EQ-VAS analysis
AMP_KNEES_CLEANED$EQVAS_FILTER<-if_else(AMP_KNEES_CLEANED$EQVAS_POSTOP_TYPE == "12MONTHS_ONLY",
                                        "NO",
                                        "YES")
AMP_KNEES_CLEANED %>% group_by(EQVAS_FILTER) %>% summarise(N = n())
rm(g5,g6,g7,g8,EQVAS_POSTOP.p, EQVAS_POSTOP12M.p, EQVAS_POSTOP6M.p)
#.........................................................................................
#.........................................................................................

# Create OKS_POSTOP_TOTSCORE var ------------------------------------------
### Create OKS_POSTOP_TOTSCORE variable (use post-op 6months, if not present use post-op  12months)
AMP_KNEES_CLEANED$OKS_POSTOP_TOTSCORE<-ifelse(AMP_KNEES_CLEANED$OKS_POSTOP_TOTSCORE_TYPE ==
                                                "12MONTHS_ONLY", #condition
                                              AMP_KNEES_CLEANED$OKS_POSTOP12M_TOTSCORE, #then
                                              AMP_KNEES_CLEANED$OKS_POSTOP6M_TOTSCORE #else
) 
#.........................................................................................
#.........................................................................................

# Create EQ5D_POSTOP_VAS var ------------------------------------------
### Create EQ5D_POSTOP_VAS variable (use post-op 6months only)
AMP_KNEES_CLEANED$EQ5D_POSTOP_VAS<-ifelse(AMP_KNEES_CLEANED$EQVAS_POSTOP_TYPE ==
                                            "12MONTHS_ONLY", #condition
                                          NA, #then
                                          AMP_KNEES_CLEANED$EQ5D_POSTOP6M_VAS #else
)  
#sapply(AMP_KNEES_CLEANED, function(x){sum(is.na(x))})
#.........................................................................................
#.........................................................................................

# Create AGEBAND var ------------------------------------------------------
###convert ages into age bands
AMP_KNEES_CLEANED$AGEBAND<-NA
AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGE <= 19,]$AGEBAND<-"0 to 19"
AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGE > 19 & AMP_KNEES_CLEANED$AGE <= 29,]$AGEBAND<-"20 to 29"
AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGE > 29 & AMP_KNEES_CLEANED$AGE <= 39,]$AGEBAND<-"30 to 39"
AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGE > 39 & AMP_KNEES_CLEANED$AGE <= 49,]$AGEBAND<-"40 to 49"
AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGE > 49 & AMP_KNEES_CLEANED$AGE <= 59,]$AGEBAND<-"50 to 59"
AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGE > 59 & AMP_KNEES_CLEANED$AGE <= 69,]$AGEBAND<-"60 to 69"
AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGE > 69 & AMP_KNEES_CLEANED$AGE <= 79,]$AGEBAND<-"70 to 79"
AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGE > 79 & AMP_KNEES_CLEANED$AGE <= 89,]$AGEBAND<-"80 to 89"
AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGE > 89,]$AGEBAND<-"90 to 120"
#AMP_KNEES_CLEANED %>% group_by(AGEBAND) %>% summarise(Min = min(AGE),Max = max(AGE))
#table(AMP_KNEES_CLEANED$AGEBAND)
# Remove two cases with AGEBAND = "30 to 39" (this age-band is not included in English data)
AMP_KNEES_CLEANED2<-AMP_KNEES_CLEANED[AMP_KNEES_CLEANED$AGEBAND != "30 to 39", ]
#nrow(AMP_KNEES_CLEANED2) ## 353
#.........................................................................................
#.........................................................................................

# Convert OKS dimensions into numbers -------------------------------------
AMP_KNEES_CLEANED3<-AMP_KNEES_CLEANED2
##*01
## OKS_PREOP_PAIN
# 0 = Severe
# 1 = Moderate
# 2 = Mild
# 3 = Very mild
# 4 = None
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_PAIN) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_PAIN=="Severe",]$OKS_PREOP_PAIN<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_PAIN=="Moderate",]$OKS_PREOP_PAIN<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_PAIN=="Mild",]$OKS_PREOP_PAIN<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_PAIN=="Very mild",]$OKS_PREOP_PAIN<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_PAIN=="None",]$OKS_PREOP_PAIN<-4
AMP_KNEES_CLEANED3$OKS_PREOP_PAIN<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_PAIN)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_PAIN) %>% summarise(N = n())
##*02
## OKS_PREOP_NIGHT_PAIN
# 0 = "Every night"
# 1 = "Most nights"
# 2 = "Some nights"
# 3 = "Only 1 or 2 nights"
# 4 = "No nights"   
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_NIGHT_PAIN) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_NIGHT_PAIN=="Every night",]$OKS_PREOP_NIGHT_PAIN<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_NIGHT_PAIN=="Most nights",]$OKS_PREOP_NIGHT_PAIN<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_NIGHT_PAIN=="Some nights",]$OKS_PREOP_NIGHT_PAIN<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_NIGHT_PAIN=="Only 1 or 2 nights",]$OKS_PREOP_NIGHT_PAIN<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_NIGHT_PAIN=="No nights",]$OKS_PREOP_NIGHT_PAIN<-4
AMP_KNEES_CLEANED3$OKS_PREOP_NIGHT_PAIN<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_NIGHT_PAIN)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_NIGHT_PAIN) %>% summarise(N = n())
##*03
## OKS_PREOP_WASHING
# 0 = "Impossible to do"
# 1 = "Extreme difficulty"
# 2 = "Moderate trouble"  
# 3 = "Very little trouble" 
# 4 = "No trouble at all"  
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_WASHING) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WASHING=="Impossible to do",]$OKS_PREOP_WASHING<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WASHING=="Extreme difficulty",]$OKS_PREOP_WASHING<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WASHING=="Moderate trouble",]$OKS_PREOP_WASHING<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WASHING=="Very little trouble",]$OKS_PREOP_WASHING<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WASHING=="No trouble at all",]$OKS_PREOP_WASHING<-4
AMP_KNEES_CLEANED3$OKS_PREOP_WASHING<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_WASHING)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_WASHING) %>% summarise(N = n())
##*04
## OKS_PREOP_TRANSPORT
# 0 = Impossible to do
# 1 = Extreme difficulty
# 2 = Moderate trouble
# 3 = Very little trouble
# 4 = No trouble at all
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_TRANSPORT) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_TRANSPORT=="Impossible to do",]$OKS_PREOP_TRANSPORT<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_TRANSPORT=="Extreme difficulty ",]$OKS_PREOP_TRANSPORT<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_TRANSPORT=="Moderate trouble",]$OKS_PREOP_TRANSPORT<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_TRANSPORT=="Very little trouble",]$OKS_PREOP_TRANSPORT<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_TRANSPORT=="No trouble at all",]$OKS_PREOP_TRANSPORT<-4
AMP_KNEES_CLEANED3$OKS_PREOP_TRANSPORT<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_TRANSPORT)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_TRANSPORT) %>% summarise(N = n())
##*05
## OKS_PREOP_WALKING
# 0 = "Not at all/pain severe when walking"
# 1 = "Around the house only"
# 2 = "5 to 15 minutes"  
# 3 = "16 to 30 minutes" 
# 4 = "No pain/more than 30 minutes"  
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_WALKING) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WALKING=="Not at all/Pain severe when walking",]$OKS_PREOP_WALKING<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WALKING=="Around the house only",]$OKS_PREOP_WALKING<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WALKING=="5 to 15 minutes",]$OKS_PREOP_WALKING<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WALKING=="16 to 30 minutes",]$OKS_PREOP_WALKING<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WALKING=="No pain/More than 30 minutes",]$OKS_PREOP_WALKING<-4
AMP_KNEES_CLEANED3$OKS_PREOP_WALKING<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_WALKING)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_WALKING) %>% summarise(N = n())
##*06
## OKS_PREOP_STANDING
# 0 = "Unbearable"
# 1 = "Very painful"
# 2 = "Moderately painful"  
# 3 = "Slightly painful" 
# 4 = "Not at all painful"  
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_STANDING) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STANDING=="Unbearable",]$OKS_PREOP_STANDING<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STANDING=="Very painful",]$OKS_PREOP_STANDING<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STANDING=="Moderately painful",]$OKS_PREOP_STANDING<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STANDING=="Slightly painful",]$OKS_PREOP_STANDING<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STANDING=="Not at all painful",]$OKS_PREOP_STANDING<-4
AMP_KNEES_CLEANED3$OKS_PREOP_STANDING<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_STANDING)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_STANDING) %>% summarise(N = n())
##*07
## OKS_PREOP_LIMPING
# 0 = All of the time
# 1 = Most of the time
# 2 = Often, not just at first
# 3 = Sometimes or just at first
# 4 = Rarely/Never
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_LIMPING) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_LIMPING=="All of the time",]$OKS_PREOP_LIMPING<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_LIMPING=="Most of the time",]$OKS_PREOP_LIMPING<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_LIMPING=="Often, not just at first",]$OKS_PREOP_LIMPING<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_LIMPING=="Sometimes or just at first",]$OKS_PREOP_LIMPING<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_LIMPING=="Rarely / never",]$OKS_PREOP_LIMPING<-4
AMP_KNEES_CLEANED3$OKS_PREOP_LIMPING<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_LIMPING)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_LIMPING) %>% summarise(N = n())
##*08
## OKS_PREOP_KNEELING
# 0 = "No, impossible"
# 1 = "With extreme difficulty"
# 2 = "With moderate difficulty"  
# 3 = "With little difficulty" 
# 4 = "Yes, easily"  
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_KNEELING) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_KNEELING=="No, impossible",]$OKS_PREOP_KNEELING<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_KNEELING=="With extreme difficulty",]$OKS_PREOP_KNEELING<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_KNEELING=="With moderate difficulty",]$OKS_PREOP_KNEELING<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_KNEELING=="With little difficulty",]$OKS_PREOP_KNEELING<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_KNEELING=="Yes, easily",]$OKS_PREOP_KNEELING<-4
AMP_KNEES_CLEANED3$OKS_PREOP_KNEELING<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_KNEELING)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_KNEELING) %>% summarise(N = n())
##*09
## OKS_PREOP_WORK
# 0 = Totally
# 1 = Greatly
# 2 = Moderately
# 3 = A little bit
# 4 = Not at all
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_WORK) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WORK=="Totally",]$OKS_PREOP_WORK<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WORK=="Greatly",]$OKS_PREOP_WORK<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WORK=="Moderate",]$OKS_PREOP_WORK<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WORK=="A little bit",]$OKS_PREOP_WORK<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_WORK=="Not at all",]$OKS_PREOP_WORK<-4
AMP_KNEES_CLEANED3$OKS_PREOP_WORK<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_WORK)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_WORK) %>% summarise(N = n())
##*10
## OKS_PREOP_CONFIDENCE
# 0 = "All the time"
# 1 = "Most of the time"
# 2 = "Often, not just at first"
# 3 = "Sometimes, or just at first"
# 4 = "Rarely/never"  
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_CONFIDENCE) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_CONFIDENCE=="All of the time",]$OKS_PREOP_CONFIDENCE<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_CONFIDENCE=="Most of the time",]$OKS_PREOP_CONFIDENCE<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_CONFIDENCE=="Often, not just at first",]$OKS_PREOP_CONFIDENCE<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_CONFIDENCE=="Sometimes, or just at first ",]$OKS_PREOP_CONFIDENCE<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_CONFIDENCE=="Rarely / never",]$OKS_PREOP_CONFIDENCE<-4
AMP_KNEES_CLEANED3$OKS_PREOP_CONFIDENCE<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_CONFIDENCE)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_CONFIDENCE) %>% summarise(N = n())
##*11
## OKS_PREOP_SHOPPING
# 0 = "No, impossible"
# 1 = "With extreme difficulty"
# 2 = "With moderate difficulty"
# 3 = "With little difficulty"
# 4 = "Yes, easily" 
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_SHOPPING) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_SHOPPING=="No, impossible",]$OKS_PREOP_SHOPPING<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_SHOPPING=="With extreme difficulty",]$OKS_PREOP_SHOPPING<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_SHOPPING=="With moderate difficulty",]$OKS_PREOP_SHOPPING<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_SHOPPING=="With little difficulty",]$OKS_PREOP_SHOPPING<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_SHOPPING=="Yes, easily",]$OKS_PREOP_SHOPPING<-4
AMP_KNEES_CLEANED3$OKS_PREOP_SHOPPING<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_SHOPPING)
AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_SHOPPING) %>% summarise(N = n())
##*12
## OKS_PREOP_STAIRS
# 0 = "No, impossible"
# 1 = "With extreme difficulty"
# 2 = "With moderate difficulty"
# 3 = "With little difficulty"
# 4 = "Yes, easily" 
#AMP_KNEES_CLEANED2 %>% group_by(OKS_PREOP_STAIRS) %>% summarise(N = n())
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STAIRS=="No, impossible",]$OKS_PREOP_STAIRS<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STAIRS=="With extreme difficulty",]$OKS_PREOP_STAIRS<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STAIRS=="With moderate difficulty",]$OKS_PREOP_STAIRS<-2
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STAIRS=="With little difficulty",]$OKS_PREOP_STAIRS<-3
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$OKS_PREOP_STAIRS=="Yes, easily",]$OKS_PREOP_STAIRS<-4
AMP_KNEES_CLEANED3$OKS_PREOP_STAIRS<-as.integer(AMP_KNEES_CLEANED3$OKS_PREOP_STAIRS)
#AMP_KNEES_CLEANED3 %>% group_by(OKS_PREOP_STAIRS) %>% summarise(N = n())
#sapply(AMP_KNEES_CLEANED2, function(x){sum(is.na(x))}) ##correct
#.........................................................................................
#.........................................................................................

# Harmonise vars to English data ------------------------------------------
##REVISION FIELD
#table(AMP_KNEES_CLEANED3$REVISION)
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$REVISION == 'primary',]$REVISION<-0
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$REVISION == 'revision',]$REVISION<-1
#table(AMP_KNEES_CLEANED3$REVISION)
##SEX FIELD
#table(AMP_KNEES_CLEANED3$SEX)
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$SEX == 'M',]$SEX<-1
AMP_KNEES_CLEANED3[AMP_KNEES_CLEANED3$SEX == 'F',]$SEX<-2
#table(AMP_KNEES_CLEANED3$SEX)
#.........................................................................................
#.........................................................................................

# Creating outcome variables MCID -----------------------------------------
##Create OKS & EQ-VAS MCID
#check std dev of EQVAS
#AMP_KNEES_CLEANED3 %>% summarise(Mean = mean(EQ5D_PREOP_VAS),stdDev = sd(EQ5D_PREOP_VAS))
##Create OKS & EQ-VAS MCID
AMP_KNEES_CLEANED3 <- as.data.frame(
  AMP_KNEES_CLEANED3 %>% 
    mutate(OKS_TOTSCORE.diff = OKS_POSTOP_TOTSCORE - OKS_PREOP_TOTSCORE) %>%
    mutate(VAS_TOTSCORE.diff = EQ5D_POSTOP_VAS - EQ5D_PREOP_VAS) %>%
    # threshold value from literature 
    # (postOKS - preOKS >= 8 points of increase)
    mutate(OKS_MCID = ifelse(OKS_TOTSCORE.diff>=7, 1,0))%>%
    # threshold value from pre-op EQ-VAS distributions
    # (postvas - preVAS >= 11 points of increase ---> 10 is 0.5*std_dev(preVAS))
    # std_dev(preVAS) --> --> this value has been calculated for English Training set
    mutate(VAS_MCID = ifelse(VAS_TOTSCORE.diff>=10, 1,0)))
rm(AMP_KNEES_PRIM.cleaned5,AMP_KNEES_REV.cleaned5,AMP_KNEES_CLEANED,AMP_KNEES_CLEANED2)
#.........................................................................................
#.........................................................................................

# Vars factor transformation ----------------------------------------------
AMP_KNEES_CLEANED3[,c("REVISION","AGEBAND","SEX")] <-
  lapply(AMP_KNEES_CLEANED3[,c("REVISION","AGEBAND","SEX")], as.factor) 
#.........................................................................................
#.........................................................................................

# Reduce sample size for knee EQVAS ----------------------------------------------
AMP_KNEES_CLEANED3.small<-AMP_KNEES_CLEANED3[!is.na(AMP_KNEES_CLEANED3$VAS_MCID),]
#nrow(AMP_KNEES_CLEANED3.small) #292
#.........................................................................................
#.........................................................................................

# Extract demographics ----------------------------------------------------
DescriptiveCatBasic.OKS(input_dataset = AMP_KNEES_CLEANED3, 
                    output_table = Descr_Categ.AMP_KNEES_CLEANED3)
DescriptiveCont2.OKS(input_dataset = AMP_KNEES_CLEANED3, 
                 output_table = Descr_Continuous.AMP_KNEES_CLEANED3)
write.csv(Descr_Categ.AMP_KNEES_CLEANED3,"output/thesis_files/Descr_Categ.AMP_KNEES_CLEANED3.csv", row.names = FALSE)
write.csv(Descr_Continuous.AMP_KNEES_CLEANED3,"output/thesis_files/Descr_Continuous.AMP_KNEES_CLEANED3.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Extract demographics for reduced dataset----------------------------------------------------
DescriptiveCatBasic.OKS(input_dataset = AMP_KNEES_CLEANED3.small, 
                    output_table = Descr_Categ.AMP_KNEES_CLEANED3.small)
DescriptiveCont2.OKS(input_dataset = AMP_KNEES_CLEANED3.small, 
                 output_table = Descr_Continuous.AMP_KNEES_CLEANED3.small)

write.csv(Descr_Categ.AMP_KNEES_CLEANED3.small,"output/thesis_files/Descr_Categ.AMP_KNEES_CLEANED3.small.csv", row.names = FALSE)
write.csv(Descr_Continuous.AMP_KNEES_CLEANED3.small,"output/thesis_files/Descr_Continuous.AMP_KNEES_CLEANED3.small.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Plots -------------------------------------------------------------------
#### Plotting distributions for differences (PostOp - PreOp) in OKS and EQ-VAS
#### including MCID thresholding for Welsh dataset
## Histogram of difference [OKS PostOp - PreOp]
Hist_OKS_test.AMPLITUDE<-ggplot(AMP_KNEES_CLEANED3, aes(x = OKS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("PostOperative OKS change\n(Welsh Knee Test Set)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("Difference OKS PostOp - PreOp")+
  scale_x_continuous(limits=c(-48,48),breaks = seq(-48,48,4)) +
  geom_vline(xintercept=7, colour="red", linetype = "longdash", size = 1)  +
  annotate(x=0,y=+Inf,label="MCID\nthreshold",vjust=2,geom="text", colour="red", size = 4)
## Histogram of difference [EQ-VAS PostOp - PreOp]
Hist_VAS_test.AMPLITUDE<-ggplot(AMP_KNEES_CLEANED3, aes(x = VAS_TOTSCORE.diff)) + 
  geom_histogram(binwidth=1, fill = "grey", colour="black")+
  ggtitle("PostOperative EQ-VAS change\n(Welsh Knee Test Set)")+
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
  geom_vline(xintercept=10, colour="red", linetype = "longdash", size = 1)  + #check the threshold line is plotted at std dev*0.5
  annotate(x=30,y=+Inf,label="MCID\nthreshold",vjust=2,geom="text", colour="red", size = 4)
## Histogram of preOp OKS
Hist_preopOKS_AMP_KNEES_CLEANED3<-ggplot(AMP_KNEES_CLEANED3, aes(x = OKS_PREOP_TOTSCORE)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PreOperative OKS tot score\n(Welsh Knee Test Set)")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10),
        axis.title=element_text(size=14))+
  ylab("N of subjects") +
  xlab("PreOperative OKS total score ")+
  scale_x_continuous(limits=c(0,48),breaks = seq(0,48,4))
## Histogram of preOp EQ-VAS
Hist_preopVAS_AMP_KNEES_CLEANED3<-ggplot(AMP_KNEES_CLEANED3, aes(x = EQ5D_PREOP_VAS)) + 
  geom_histogram(binwidth=1,
                 fill = "grey", colour="black")+
  ggtitle("PreOperative EQ-VAS score\n(Welsh Knee Test Set)")+
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











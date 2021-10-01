###### PRE-PROCESSING - Welsh Hips dataset (PRIMARY & REVISION)


### Import libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
source("code/00_ProjectFunctions.R") # library of custom-made functions


### Import input data -------------------------------------------------------
AMP_HIPS_PRIM<-read.csv("input_data/Welsh_data/Amplitude_HipsPrimary190404_forAuraFrizzatiMSc_anonRP210414.csv", 
                        header = T,
                        na.strings=c("", "I", "DUE", "OVERDUE", "NA"))

AMP_HIPS_REV<-read.csv("input_data/Welsh_data/Amplitude_HipsRevision170803_forAuraFrizzatiMSc_anonRP210414.csv", 
                       header = T,
                       na.strings=c("", "I", "DUE", "OVERDUE", "NA"))




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



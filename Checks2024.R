# Import libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)


# Run 01_Preprocess_WelshHips.R, to create master tables used in paper: ---------------------------------------------------------------
# used for EQ-VAS --> AMP_HIPS_CLEANED_3 (post-operative EQVAS 6-months and 12-months postop)
# OHS --> AMP_HIPS_CLEANED_3.small (post-operative OHS 6-months postop only)



# Import input data -------------------------------------------------------
AMP_HIPS_PRIM<-read.csv("input_data/Welsh_data/Amplitude_HipsPrimary190404_forAuraFrizzatiMSc_anonRP210414.csv", 
                        header = T,
                        na.strings=c("", "I", "DUE", "OVERDUE", "NA"))

AMP_HIPS_REV<-read.csv("input_data/Welsh_data/Amplitude_HipsRevision170803_forAuraFrizzatiMSc_anonRP210414.csv", 
                       header = T,
                       na.strings=c("", "I", "DUE", "OVERDUE", "NA"))

# create a tables that contain PIDS and dates and attach it as a lookup -------------------------------------------------------

## Dates lookup for primary scores
AMP_HIPS_PRIMARY_PIDs_and_dates <-
  AMP_HIPS_PRIM |>
  mutate(
    ## only the postop 6-months OHS scores were used
    OHS_PREOP_DATE = as.Date(`Completed.Date...OHS.Oxford.Hip.Score...Score...Baseline`,format = "%d/%m/%Y")
    ,OHS_POSTOP6M_DATE = as.Date(`Completed.Date...OHS.Oxford.Hip.Score...Score...6.Months`,format = "%d/%m/%Y")
    # ,OHS_POSTOP12M_DATE = as.Date(`Completed.Date...OHS.Oxford.Hip.Score...Score...12.Months",format = "%d/%m/%Y")
    
    ## both the postop 6-months and 12-months EQVAS scores were used
    ,EQ5D_PREOP_VAS_DATE = as.Date(`Completed.Date...EQ.5D.5L...Health.VAS...Baseline`,format = "%d/%m/%Y")
    ,EQ5D_POSTOP6M_VAS_DATE = as.Date(`Completed.Date...EQ.5D.5L...Health.VAS...6.Months`,format = "%d/%m/%Y")
    ,EQ5D_POSTOP12M_VAS_DATE = as.Date(`Completed.Date...EQ.5D.5L...Health.VAS...12.Months`,format = "%d/%m/%Y")
  ) |>
  select(pID, 
         OHS_PREOP_DATE, OHS_POSTOP6M_DATE, 
         EQ5D_PREOP_VAS_DATE, EQ5D_POSTOP6M_VAS_DATE, EQ5D_POSTOP12M_VAS_DATE)

## Dates lookup for revision scores
AMP_HIPS_REVISION_PIDs_and_dates <-
  AMP_HIPS_REV |>
  mutate(
    ## only the postop 6-months OHS scores were used
    OHS_PREOP_DATE = as.Date(`Completed.Date...Oxford.Hip.Score...Score...Baseline`,format = "%d/%m/%Y")
    ,OHS_POSTOP6M_DATE = as.Date(`Completed.Date...Oxford.Hip.Score...Score...6.Months`,format = "%d/%m/%Y")
    # ,OHS_POSTOP12M_DATE = as.Date(`Completed.Date...Oxford.Hip.Score...Score...12.Months",format = "%d/%m/%Y")
    
    ## both the postop 6-months and 12-months EQVAS scores were used
    ,EQ5D_PREOP_VAS_DATE = as.Date(`Completed.Date...EQ.5D.5L...Health.VAS...Baseline`,format = "%d/%m/%Y")
    ,EQ5D_POSTOP6M_VAS_DATE = as.Date(`Completed.Date...EQ.5D.5L...Health.VAS...6.Months`,format = "%d/%m/%Y")
    ,EQ5D_POSTOP12M_VAS_DATE = as.Date(`Completed.Date...EQ.5D.5L...Health.VAS...12.Months`,format = "%d/%m/%Y")
  ) |>
  select(pID, 
         OHS_PREOP_DATE, OHS_POSTOP6M_DATE, 
         EQ5D_PREOP_VAS_DATE, EQ5D_POSTOP6M_VAS_DATE, EQ5D_POSTOP12M_VAS_DATE)
  


# join master tables to Dates lookup --------------------------------------

### EQVAS
## AMP_HIPS_CLEANED_3 (EQVAS measures, 6 and 12-months postop)

AMP_HIPS_CLEANED_3_withDates <-
  rbind(
  ## revision EQVAS
  AMP_HIPS_CLEANED_3 |>
  filter(REVISION == 1) |>
  left_join(AMP_HIPS_REVISION_PIDs_and_dates |> 
              filter(!is.na(EQ5D_PREOP_VAS_DATE)),
            by = "pID"
            ,relationship = "many-to-many"), ## to take into account people with multiple surgeries left and right
  
  ## primary EQVAS
  AMP_HIPS_CLEANED_3 |>
  filter(REVISION == 0) |>
  left_join(AMP_HIPS_PRIMARY_PIDs_and_dates |> 
              filter(!is.na(EQ5D_PREOP_VAS_DATE)),
            by = "pID"
            ,relationship = "many-to-many" ## to take into account people with multiple surgeries left and right
            )
  ) |>
  mutate(
    ## use 6-months postop eqvas date if not NA, otherwise use 12-months postop eqvas
    EQVAS_POSTOP_DATE = if_else(!is.na(EQ5D_POSTOP6M_VAS_DATE),
                                EQ5D_POSTOP6M_VAS_DATE,
                                EQ5D_POSTOP12M_VAS_DATE)
    ,DAYSDIFF_POSTOP_BASELINE_EQVAS = EQVAS_POSTOP_DATE-EQ5D_PREOP_VAS_DATE
    # I check whether the difference is > 6 months, which is at least 28+31+30+31+30+31 = 181 days
    ,CORRECT_POSTOP_DATEDIFF = if_else(DAYSDIFF_POSTOP_BASELINE_EQVAS >= 181, TRUE, FALSE)
    ,NEGATIVE_POSTOP_DATEDIFF = if_else(DAYSDIFF_POSTOP_BASELINE_EQVAS < 0, "negative", "correct")
  )

## How many records have postop before 6 months? (number of FALSE)
AMP_HIPS_CLEANED_3_withDates |>
  group_by(CORRECT_POSTOP_DATEDIFF) |>
  summarise(n = n())

## How many records have postop recorded before preop? (number of "negative")
AMP_HIPS_CLEANED_3_withDates |>
  group_by(NEGATIVE_POSTOP_DATEDIFF) |>
  summarise(n = n())

## we can correct the paper having the minimum number of post-op days available
AMP_HIPS_CLEANED_3_withDates |>
  summarise(
    max_diff = max(DAYSDIFF_POSTOP_BASELINE_EQVAS,na.rm = T)
    ,min_diff = min(DAYSDIFF_POSTOP_BASELINE_EQVAS,na.rm = T)
    ,mean_diff= mean(DAYSDIFF_POSTOP_BASELINE_EQVAS,na.rm = T)
  )


### OHS
## AMP_HIPS_CLEANED_3.small (OHS measures, 6 amonths postop)

AMP_HIPS_CLEANED_3.small_withDates <-
  rbind(
    ## revision OHS
    AMP_HIPS_CLEANED_3 |>
      filter(REVISION == 1) |>
      left_join(AMP_HIPS_REVISION_PIDs_and_dates |> 
                  filter(!is.na(OHS_PREOP_DATE)),
                by = "pID"
                ,relationship = "many-to-many"), ## to take into account people with multiple surgeries left and right
    
    ## primary ohs
    AMP_HIPS_CLEANED_3 |>
      filter(REVISION == 0) |>
      left_join(AMP_HIPS_PRIMARY_PIDs_and_dates |> 
                  filter(!is.na(OHS_PREOP_DATE)),
                by = "pID"
                ,relationship = "many-to-many" ## to take into account people with multiple surgeries left and right
      )
  ) |>
  mutate(
    ## use 6-months postop eqvas date if not NA, otherwise use 12-months postop eqvas
    OHS_POSTOP_DATE = OHS_POSTOP6M_DATE
    ,DAYSDIFF_POSTOP_BASELINE_OHS = OHS_POSTOP_DATE-OHS_PREOP_DATE
    # I check whether the difference is > 6 months, which is at least 28+31+30+31+30+31 = 181 days
    ,CORRECT_POSTOP_DATEDIFF = if_else(DAYSDIFF_POSTOP_BASELINE_OHS >= 181, TRUE, FALSE)
    ,NEGATIVE_POSTOP_DATEDIFF = if_else(DAYSDIFF_POSTOP_BASELINE_OHS < 0, "negative", "correct")
  )

## How many records have postop before 6 months? (number of FALSE)
AMP_HIPS_CLEANED_3.small_withDates |>
  group_by(CORRECT_POSTOP_DATEDIFF) |>
  summarise(n = n())

## How many records have postop recorded before preop? (number of "negative")
AMP_HIPS_CLEANED_3.small_withDates |>
  group_by(NEGATIVE_POSTOP_DATEDIFF) |>
  summarise(n = n())

## we can correct the paper having the minimum number of post-op days available
AMP_HIPS_CLEANED_3.small_withDates |>
  summarise(
    max_diff = max(DAYSDIFF_POSTOP_BASELINE_OHS,na.rm = T)
    ,min_diff = min(DAYSDIFF_POSTOP_BASELINE_OHS,na.rm = T)
    ,mean_diff= mean(DAYSDIFF_POSTOP_BASELINE_OHS,na.rm = T)
  )



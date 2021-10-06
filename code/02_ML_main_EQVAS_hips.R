### Import libraries ---------------------------------------------------------------
library(caret) # Machine learning library
library(dplyr)
source("code/00_ProjectFunctions.R") # custom-made functions
#.........................................................................................
#.........................................................................................

### Import input data -------------------------------------------------------
attach("code/01_Preprocess_EnglishHips.RData")
English.training.1618 <-English.training.1618 ##import English dataset training
English.test.1819 <- English.test.1819 ##import English dataset testing
attach("code/01_Preprocess_WelshHips.RData")
AMP_HIPS_CLEANED_3<-AMP_HIPS_CLEANED_3
#.........................................................................................
#.........................................................................................


### Pre-processing input data -------------------------------------------------------
##### Training set
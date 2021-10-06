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
AMP_HIPS_CLEANED_3.small<-AMP_HIPS_CLEANED_3.small
#.........................................................................................
#.........................................................................................

### Pre-processing input data -------------------------------------------------------
##### Training set
### Setting the factor order for outcome variable OHS_MCID
English.training.1618[English.training.1618$OHS_MCID==1,]$OHS_MCID<-"YES"
English.training.1618[English.training.1618$OHS_MCID==0,]$OHS_MCID<-"NO"
### Setting the positive outcome to be YES for caret
English.training.1618$OHS_MCID <- factor(English.training.1618$OHS_MCID, levels = c("YES", "NO"))
#table(English.training.1618$OHS_MCID)
#prop.table(table(English.training.1618$OHS_MCID))
##outcome class imbalance --> to take care of it during model training via upsampling
###Select predictors for main model OHS MCID
predictors.simple<-c("REVISION",
                     "AGEBAND",
                     "SEX",
                     # "PREOP_ASSISTED",
                     # "PREOP_PREVIOUS_SURGERY",
                     # "PREOP_LIVING_ARRANGEMENTS",
                     # "PREOP_DISABILITY",
                     # "HEART_DISEASE",
                     # "HIGH_BP",
                     # "STROKE",
                     # "CIRCULATION",
                     # "LUNG_DISEASE",
                     # "DIABETES",
                     # "KIDNEY_DISEASE",
                     # "NERVOUS_SYSTEM",
                     # "LIVER_DISEASE",
                     # "CANCER",
                     # "DEPRESSION",
                     # "ARTHRITIS",
                     # "EQ5D_PREOP_MOBILITY",
                     # "EQ5D_PREOP_SELFCARE",
                     # "EQ5D_PREOP_ACTIVITY",
                     # "EQ5D_PREOP_DISCOMFORT",
                     # "EQ5D_PREOP_ANXIETY",
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
                     "OHS_PREOP_TOTSCORE",
                     "OHS_MCID"
)
## data to use for training for main model
English.training.1618.OHS.simple<-English.training.1618[predictors.simple] ##18 predictors

##### English Test set
### Setting the factor order for outcome variable OHS_MCID
English.test.1819[English.test.1819$OHS_MCID==1,]$OHS_MCID<-"YES"
English.test.1819[English.test.1819$OHS_MCID==0,]$OHS_MCID<-"NO"
### The reference level needs to be set as YES to have the probability of predicting NO
English.test.1819$OHS_MCID <- factor(English.test.1819$OHS_MCID, levels = c("YES", "NO"))
#table(English.test.1819$OHS_MCID)
#prop.table(table(English.test.1819$OHS_MCID))
## data to use for test for simple model
English.test.1819.OHS.simple<-English.test.1819[predictors.simple] ##18 predictors

##### Welsh Test set
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED_3.small[predictors.simple]
### Setting the factor order for outcome variable OHS_MCID for dataset AMP_HIPS_CLEANED3.test
AMP_HIPS_CLEANED3.test[AMP_HIPS_CLEANED3.test$OHS_MCID==1,]$OHS_MCID<-"YES"
AMP_HIPS_CLEANED3.test[AMP_HIPS_CLEANED3.test$OHS_MCID==0,]$OHS_MCID<-"NO"
### Setting the positive outcome to be YES for caret
AMP_HIPS_CLEANED3.test$OHS_MCID <- factor(AMP_HIPS_CLEANED3.test$OHS_MCID, levels = c("YES", "NO"))
#table(AMP_HIPS_CLEANED3.test$OHS_MCID)
#prop.table(table(AMP_HIPS_CLEANED3.test$OHS_MCID))
#nrow(AMP_HIPS_CLEANED3.test) ## 440

rm(English.test.1819,English.training.1618,AMP_HIPS_CLEANED_3.small, predictors.simple)
#.........................................................................................
#.........................................................................................

# Train XGBoost on OHS ----------------------------------------
attach("code/02_ML_main_OHS.RData")
English.training.1618.OHS.simple<-English.training.1618.OHS.simple
English.test.1819.OHS.simple<-English.test.1819.OHS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
#gc()
#memory.size()
##Train the model with caret
#library(caret)
set.seed(1)
Mod_XGBTREE.1618.OHS.simple = train(
  form = OHS_MCID ~ .,
  data = English.training.1618.OHS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           sampling = "up", ## outcome class is unbalanced, so up-sampling is required
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for maximising AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "xgbTree",
  metric = 'ROC' 
)
#Mod_XGBTREE.1618.OHS.simple
#### saved in 
#load("code/models/OHS_simple/HIPS_ML_training_1618_XGBTREE_simple_EngTest.RData")
#load("code/models/OHS_simple/HIPS_ML_training_1618_XGBTREE_simple_WelshTest.RData")
## selected hyperparameters
head(Mod_XGBTREE.1618.OHS.simple$results[
  order(Mod_XGBTREE.1618.OHS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_XGBTREE.1618.OHS.simple <- thresholder(Mod_XGBTREE.1618.OHS.simple,
                                               threshold = seq(0,1,0.05),
                                               final = TRUE,
                                               statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_XGBTREE.1618.OHS.simple<-ths.Mod_XGBTREE.1618.OHS.simple[ths.Mod_XGBTREE.1618.OHS.simple$J == 
                                                                                max(ths.Mod_XGBTREE.1618.OHS.simple$J),
                                                                              "prob_threshold"]
best_p_threshold.Mod_XGBTREE.1618.OHS.simple ## p = 0.5
### Variable importance
plot(caret::varImp(Mod_XGBTREE.1618.OHS.simple), top = 10, 
     main="Variable importance (OHS MCID)\nExtreme Gradient Boosting Tree (Simple model)") 
#width = 600, height = 400
#.........................................................................................
#.........................................................................................

# Test XGBoost on OHS (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_XGBTREE.1618.OHS.simple <- predict(Mod_XGBTREE.1618.OHS.simple, 
                                                 newdata = English.test.1819.OHS.simple, type = "prob")

### [4.4.2] calibration curves (caret package)
cal.Mod_XGBTREE.1618.OHS.simple.EngTest<-cbind(English.test.1819.OHS.simple$OHS_MCID,probsTest.Mod_XGBTREE.1618.OHS.simple)
names(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)<-c("OHS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest<-calibration(OHS_MCID ~ YES, data = cal.Mod_XGBTREE.1618.OHS.simple.EngTest)
cal.plot.Mod_XGBTREE.1618.OHS.simple.EngTest<-plot(cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest)
cal.ggplot.Mod_XGBTREE.1618.OHS.simple.EngTest<-ggplot(cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest)
rm(cal.Mod_XGBTREE.1618.OHS.simple.EngTest,cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest)
## Hosmer-Lemeshow calibration
#head(English.test.1819.OHS.simple$OHS_MCID) ##true outcome
#head(probsTest.Mod_XGBTREE.1618.OHS.simple)[1] ## predicted score
cal.Mod_XGBTREE.1618.OHS.simple.EngTest<-cbind(English.test.1819.OHS.simple$OHS_MCID,
                                               probsTest.Mod_XGBTREE.1618.OHS.simple[1])
names(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)<-c("True_outcome", "Prob_YES")
#head(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)
cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_logic <- ifelse(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome == "YES", TRUE, FALSE)
#head(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)
table(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome,cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_logic)
cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_num <- ifelse(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome == "YES", 1, 0)
table(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome,
      cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_num)
library(PredictABEL)
plotCalibration(data=cal.Mod_XGBTREE.1618.OHS.simple.EngTest, 
                cOutcome=4, 
                predRisk=cal.Mod_XGBTREE.1618.OHS.simple.EngTest$Prob_YES)
#head(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)
library(ResourceSelection)
hoslem.test(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$Prob_YES, ## expected/predicted
            cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_num) ## observed
#Hosmer and Lemeshow goodness of fit (GOF) test
# X-squared = 39189, df = 8, p-value < 2.2e-16
### Predict classes on test set
pred.Mod_XGBTREE.1618.OHS.simple <- factor(ifelse(probsTest.Mod_XGBTREE.1618.OHS.simple[, "YES"] > 
                                                    best_p_threshold.Mod_XGBTREE.1618.OHS.simple,
                                                  "YES", "NO")) ##should it be > or >= best_p_threshold?
pred.Mod_XGBTREE.1618.OHS.simple<-relevel(pred.Mod_XGBTREE.1618.OHS.simple, "YES")
#table(pred.Mod_XGBTREE.1618.OHS.simple)
#head(probsTest.Mod_XGBTREE.1618.OHS.simple)
#head(pred.Mod_XGBTREE.1618.OHS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_XGBTREE.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_XGBTREE.1618.OHS.simple,
  reference = English.test.1819.OHS.simple$OHS_MCID,
  positive = "YES")
#conf.Matrix.Mod_XGBTREE.1618.OHS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_XGBTREE.1618.OHS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_XGBTREE.1618.OHS.simple,
  trained.model = Mod_XGBTREE.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_XGBTREE.1618.OHS.simple,
  test_set = English.test.1819.OHS.simple,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_XGBTREE.1618.OHS.simple,
  algorithm ="XGBTREE_OHS_EngTest"
)

#METRICS_Mod_XGBTREE.1618.OHS.simple_EngTest
write.csv(METRICS_Mod_XGBTREE.1618.OHS.simple_EngTest,
          "MODELS_METRICS/METRICS_Mod_XGBTREE.1618.OHS.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test XGBoost on OHS (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_XGBTREE.1618.OHS.simple <- predict(Mod_XGBTREE.1618.OHS.simple, 
                                                 newdata = AMP_HIPS_CLEANED3.test, type = "prob")

### Calibration curves (caret package)
cal.Mod_XGBTREE.1618.OHS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$OHS_MCID,
                                                 probsTest.Mod_XGBTREE.1618.OHS.simple)
names(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)<-c("OHS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest<-caret::calibration(OHS_MCID ~ YES, 
                                                           data = cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)
cal.plot.Mod_XGBTREE.1618.OHS.simple.WelshTest<-plot(cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest)
cal.ggplot.Mod_XGBTREE.1618.OHS.simple.WelshTest<-ggplot2::ggplot(cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest)
rm(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest, cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest)

## Hosmer-Lemeshow calibration
#head(AMP_HIPS_CLEANED3.test$OHS_MCID) ##true outcome
#head(probsTest.Mod_XGBTREE.1618.OHS.simple)[1] ## predicted score
cal.Mod_XGBTREE.1618.OHS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$OHS_MCID,
                                                 probsTest.Mod_XGBTREE.1618.OHS.simple[1])
names(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)<-c("True_outcome", "Prob_YES")
#head(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)
cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_logic <- ifelse(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome == "YES", TRUE, FALSE)
#head(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)
#table(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome,cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_logic)
cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_num <- ifelse(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome == "YES", 1, 0)
# table(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome,
#       cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_num)
## Hosmer-Lemeshow calibration
ResourceSelection::hoslem.test(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$Prob_YES, ## expected/predicted
            cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_num) ## observed
#Hosmer and Lemeshow goodness of fit (GOF) test
#X-squared = 181.29, df = 8, p-value < 2.2e-16
### Predict classes on test set
pred.Mod_XGBTREE.1618.OHS.simple <- factor(ifelse(probsTest.Mod_XGBTREE.1618.OHS.simple[, "YES"] > 
                                                    best_p_threshold.Mod_XGBTREE.1618.OHS.simple,
                                                  "YES", "NO")) ##should it be > or >= best_p_threshold?
pred.Mod_XGBTREE.1618.OHS.simple<-relevel(pred.Mod_XGBTREE.1618.OHS.simple, "YES")
#table(pred.Mod_XGBTREE.1618.OHS.simple)
#head(probsTest.Mod_XGBTREE.1618.OHS.simple)
#head(pred.Mod_XGBTREE.1618.OHS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_XGBTREE.1618.OHS.simple<-caret::confusionMatrix(
  data = pred.Mod_XGBTREE.1618.OHS.simple,
  reference = AMP_HIPS_CLEANED3.test$OHS_MCID,
  positive = "YES")
#conf.Matrix.Mod_XGBTREE.1618.OHS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_XGBTREE.1618.OHS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_XGBTREE.1618.OHS.simple,
  trained.model = Mod_XGBTREE.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_XGBTREE.1618.OHS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_XGBTREE.1618.OHS.simple,
  algorithm ="XGBTREE_OHS_WelshTest"
)

METRICS_Mod_XGBTREE.1618.OHS.simple_WelshTest
write.csv(METRICS_Mod_XGBTREE.1618.OHS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_XGBTREE.1618.OHS.simple_WelshTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................



# Train Logistic Regression on OHS ----------------------------------------
attach("code/02_ML_main_OHS.RData")
English.training.1618.OHS.simple<-English.training.1618.OHS.simple
English.test.1819.OHS.simple<-English.test.1819.OHS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
##Train the model with caret
set.seed(1)
Mod_LR.1618.OHS.simple = train(
  form = OHS_MCID ~ .,
  data = English.training.1618.OHS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           sampling = "up", ## outcome class is unbalanced , so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## required for maximising AUROC metric
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "glm",
  family = "binomial",
  metric = 'ROC' 
)
#Mod_LR.1618.OHS.simple
#summary(Mod_LR.1618.OHS.simple)

#### saved in 
#load("code/models/OHS_simple/HIPS_ML_training_1618_LR_simple_EngTest.RData")
#load("code/models/OHS_simple/HIPS_ML_training_1618_LR_simple_WelshTest.RData")


###Define p threshold
ths.Mod_LR.1618.OHS.simple <- thresholder(Mod_LR.1618.OHS.simple,
                                          threshold = seq(0,1,0.05),
                                          final = TRUE,
                                          statistics = "all")

#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_LR.1618.OHS.simple<-ths.Mod_LR.1618.OHS.simple[ths.Mod_LR.1618.OHS.simple$J == 
                                                                      max(ths.Mod_LR.1618.OHS.simple$J),
                                                                    "prob_threshold"]
best_p_threshold.Mod_LR.1618.OHS.simple ## p = 0.5

### Variable importance
plot(varImp(Mod_LR.1618.OHS.simple), top = 10, 
     main="Variable importance (OHS MCID)\nLog Reg (Simple model)") 
#.........................................................................................
#.........................................................................................

# Test Logistic Regression on OHS (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_LR.1618.OHS.simple <- predict(Mod_LR.1618.OHS.simple, 
                                            newdata = English.test.1819.OHS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_LR.1618.OHS.simple.EngTest<-cbind(English.test.1819.OHS.simple$OHS_MCID,
                                          probsTest.Mod_LR.1618.OHS.simple)
names(cal.Mod_LR.1618.OHS.simple.EngTest)<-c("OHS_MCID","YES","NO")
cal.obj.Mod_LR.1618.OHS.simple.EngTest<-calibration(OHS_MCID ~ YES, 
                                                    data = cal.Mod_LR.1618.OHS.simple.EngTest)
cal.plot.Mod_LR.1618.OHS.simple.EngTest<-plot(cal.obj.Mod_LR.1618.OHS.simple.EngTest)
cal.ggplot.Mod_LR.1618.OHS.simple.EngTest<-ggplot(cal.obj.Mod_LR.1618.OHS.simple.EngTest)
rm(cal.Mod_LR.1618.OHS.simple.EngTest,cal.obj.Mod_LR.1618.OHS.simple.EngTest)
### Predict classes on test set
pred.Mod_LR.1618.OHS.simple <- factor(ifelse(probsTest.Mod_LR.1618.OHS.simple[, "YES"] > 
                                               best_p_threshold.Mod_LR.1618.OHS.simple,
                                             "YES", "NO")) 
pred.Mod_LR.1618.OHS.simple<-relevel(pred.Mod_LR.1618.OHS.simple, "YES")
#table(pred.Mod_LR.1618.OHS.simple)
#head(probsTest.Mod_LR.1618.OHS.simple)
#head(pred.Mod_LR.1618.OHS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_LR.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_LR.1618.OHS.simple,
  reference = English.test.1819.OHS.simple$OHS_MCID,
  positive = "YES")
#conf.Matrix.Mod_LR.1618.OHS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_LR.1618.OHS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_LR.1618.OHS.simple,
  trained.model = Mod_LR.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_LR.1618.OHS.simple,
  test_set = English.test.1819.OHS.simple,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_LR.1618.OHS.simple,
  algorithm ="LOGREG_OHS_EngTest"
)
#METRICS_Mod_LR.1618.OHS.simple_EngTest
write.csv(METRICS_Mod_LR.1618.OHS.simple_EngTest,
          "MODELS_METRICS/METRICS_Mod_LR.1618.OHS.simple_EngTest.csv", row.names = FALSE) 
#.........................................................................................
#.........................................................................................

# Test Logistic Regression on OHS (Welsh Test set)----------------------------------------
###Predicting probability scores on Welsh test set
probsTest.Mod_LR.1618.OHS.simple <- predict(Mod_LR.1618.OHS.simple, 
                                            newdata = AMP_HIPS_CLEANED3.test, type = "prob")
### calibration curves (caret package)
cal.Mod_LR.1618.OHS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$OHS_MCID,
                                            probsTest.Mod_LR.1618.OHS.simple)
names(cal.Mod_LR.1618.OHS.simple.WelshTest)<-c("OHS_MCID","YES","NO")
cal.obj.Mod_LR.1618.OHS.simple.WelshTest<-calibration(OHS_MCID ~ YES, 
                                                      data = cal.Mod_LR.1618.OHS.simple.WelshTest)
cal.plot.Mod_LR.1618.OHS.simple.WelshTest<-plot(cal.obj.Mod_LR.1618.OHS.simple.WelshTest)
cal.ggplot.Mod_LR.1618.OHS.simple.WelshTest<-ggplot(cal.obj.Mod_LR.1618.OHS.simple.WelshTest)
rm(cal.Mod_LR.1618.OHS.simple.WelshTest,cal.obj.Mod_LR.1618.OHS.simple.WelshTest)
### Predict classes on test set
pred.Mod_LR.1618.OHS.simple <- factor(ifelse(probsTest.Mod_LR.1618.OHS.simple[, "YES"] > 
                                               best_p_threshold.Mod_LR.1618.OHS.simple,
                                             "YES", "NO")) 
pred.Mod_LR.1618.OHS.simple<-relevel(pred.Mod_LR.1618.OHS.simple, "YES")
#table(pred.Mod_LR.1618.OHS.simple)
#head(probsTest.Mod_LR.1618.OHS.simple)
#head(pred.Mod_LR.1618.OHS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_LR.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_LR.1618.OHS.simple,
  reference = AMP_HIPS_CLEANED3.test$OHS_MCID,
  positive = "YES")
#conf.Matrix.Mod_LR.1618.OHS.simple$table


### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_LR.1618.OHS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_LR.1618.OHS.simple,
  trained.model = Mod_LR.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_LR.1618.OHS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_LR.1618.OHS.simple,
  algorithm ="LOGREG_OHS_WelshTest"
)

write.csv(METRICS_Mod_LR.1618.OHS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_LR.1618.OHS.simple_WelshTest.csv", row.names = FALSE) 
#.........................................................................................
#.........................................................................................

# Train Neural Net on OHS ----------------------------------------
attach("code/02_ML_main_OHS.RData")
English.training.1618.OHS.simple<-English.training.1618.OHS.simple
English.test.1819.OHS.simple<-English.test.1819.OHS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
##Train the model with caret
set.seed(1)
Mod_NNET.1618.OHS.simple = train(
  form = OHS_MCID ~ .,
  data = English.training.1618.OHS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, ## are these the best values to use for repeated cv?
                           sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "nnet",
  metric = 'ROC' 
)
#Mod_NNET.1618.OHS.simple
load("code/models/OHS_simple/HIPS_ML_training_1618_NNET_simple_EngTest.RData")
load("code/models/OHS_simple/HIPS_ML_training_1618_NNET_simple_WelshTest.RData")
## selected hyperparameters
head(Mod_NNET.1618.OHS.simple$results[
  order(Mod_NNET.1618.OHS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_NNET.1618.OHS.simple <- thresholder(Mod_NNET.1618.OHS.simple,
                                            threshold = seq(0,1,0.05),
                                            final = TRUE,
                                            statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_NNET.1618.OHS.simple<-ths.Mod_NNET.1618.OHS.simple[ths.Mod_NNET.1618.OHS.simple$J == 
                                                                          max(ths.Mod_NNET.1618.OHS.simple$J),
                                                                        "prob_threshold"]
best_p_threshold.Mod_NNET.1618.OHS.simple ## p = 0.5
### Variable importance
plot(varImp(Mod_NNET.1618.OHS.simple), top = 10, 
     main="Variable importance (OHS MCID)\nNeural Net (Simple model)") 
#.........................................................................................
#.........................................................................................

# Test Neural Net on OHS (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_NNET.1618.OHS.simple <- predict(Mod_NNET.1618.OHS.simple, 
                                              newdata = English.test.1819.OHS.simple, type = "prob")
### Predict classes on test set
pred.Mod_NNET.1618.OHS.simple <- factor(ifelse(probsTest.Mod_NNET.1618.OHS.simple[, "YES"] > 
                                                 best_p_threshold.Mod_NNET.1618.OHS.simple,
                                               "YES", "NO")) 
pred.Mod_NNET.1618.OHS.simple<-relevel(pred.Mod_NNET.1618.OHS.simple, "YES")
table(pred.Mod_NNET.1618.OHS.simple)
# YES    NO 
# 22301  7358  
head(probsTest.Mod_NNET.1618.OHS.simple)
#        YES        NO
# 300492 0.4847598 0.5152402
# 300493 0.7827850 0.2172150
# 300495 0.6939932 0.3060068
# 300496 0.5603096 0.4396904
# 300497 0.6650111 0.3349889
# 300498 0.6421050 0.3578950
head(pred.Mod_NNET.1618.OHS.simple) 
# [1] NO YES YES YES YES YES
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_NNET.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_NNET.1618.OHS.simple,
  reference = English.test.1819.OHS.simple$OHS_MCID,
  positive = "YES")
conf.Matrix.Mod_NNET.1618.OHS.simple$table
#              Reference
# Prediction   YES    NO
# YES        21336   965
# NO          5892  1466
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_NNET.1618.OHS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_NNET.1618.OHS.simple,
  trained.model = Mod_NNET.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_NNET.1618.OHS.simple,
  test_set = English.test.1819.OHS.simple,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_NNET.1618.OHS.simple,
  algorithm ="NNET_OHS_EngTest"
)
#METRICS_Mod_NNET.1618.OHS.simple_EngTest
write.csv(METRICS_Mod_NNET.1618.OHS.simple_EngTest,
          "MODELS_METRICS/METRICS_Mod_NNET.1618.OHS.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test Neural Net on OHS (Welsh Test set)----------------------------------------

### Predicting probability scores on Welsh test set
probsTest.Mod_NNET.1618.OHS.simple <- predict(Mod_NNET.1618.OHS.simple, 
                                              newdata = AMP_HIPS_CLEANED3.test, type = "prob")
### [5.5.3] Predict classes on test set
pred.Mod_NNET.1618.OHS.simple <- factor(ifelse(probsTest.Mod_NNET.1618.OHS.simple[, "YES"] > 
                                                 best_p_threshold.Mod_NNET.1618.OHS.simple,
                                               "YES", "NO")) 
pred.Mod_NNET.1618.OHS.simple<-relevel(pred.Mod_NNET.1618.OHS.simple, "YES")
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_NNET.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_NNET.1618.OHS.simple,
  reference = AMP_HIPS_CLEANED3.test$OHS_MCID,
  positive = "YES")
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_NNET.1618.OHS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_NNET.1618.OHS.simple,
  trained.model = Mod_NNET.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_NNET.1618.OHS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_NNET.1618.OHS.simple,
  algorithm ="NNET_OHS_WelshTest"
)
write.csv(METRICS_Mod_NNET.1618.OHS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_NNET.1618.OHS.simple_WelshTest.csv", row.names = FALSE) 
#.........................................................................................
#.........................................................................................

# Train Random Forest on OHS ----------------------------------------
attach("code/02_ML_main_OHS.RData")
English.training.1618.OHS.simple<-English.training.1618.OHS.simple
English.test.1819.OHS.simple<-English.test.1819.OHS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
##Train the model with caret
#### [6.1] Model OHS SIMPLE: RANDOM FOREST (ordinal vars as continuous))
set.seed(1)
Mod_RF.1618.OHS.simple = train(
  form = OHS_MCID ~ .,
  data = English.training.1618.OHS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, ## are these the best values to use for repeated cv?
                           sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "rf",
  metric = 'ROC' # I am not sure this actually gets used for glm
)
#Mod_RF.1618.OHS.simple
source("code/models/OHS_simple/HIPS_ML_training_1618_RF_simple_EngTest.RData")
source("code/models/OHS_simple/HIPS_ML_training_1618_RF_simple_WelshTest.RData")
## selected hyperparameters
head(Mod_RF.1618.OHS.simple$results[
  order(Mod_RF.1618.OHS.simple$results$ROC, decreasing = TRUE),],1)
### [6.2] Define p threshold
ths.Mod_RF.1618.OHS.simple <- thresholder(Mod_RF.1618.OHS.simple,
                                          threshold = seq(0,1,0.05),
                                          final = TRUE,
                                          statistics = "all")

values_ths<-c("prob_threshold", "Sensitivity", "Specificity", "J","Balanced Accuracy", 
              "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1", 
              "Prevalence", "Detection Rate", "Detection Prevalence", "Accuracy", 
              "Kappa", "Dist")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_RF.1618.OHS.simple<-ths.Mod_RF.1618.OHS.simple[ths.Mod_RF.1618.OHS.simple$J == 
                                                                      max(ths.Mod_RF.1618.OHS.simple$J),
                                                                    "prob_threshold"]
best_p_threshold.Mod_RF.1618.OHS.simple ## p = 0.7
### Variable importance
plot(varImp(Mod_RF.1618.OHS.simple), top = 10, 
     main="Variable importance (OHS MCID)\nRandom Forest (Simple model)")  
#.........................................................................................
#.........................................................................................

# Test Random Forest on OHS (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_RF.1618.OHS.simple <- predict(Mod_RF.1618.OHS.simple, 
                                            newdata = English.test.1819.OHS.simple, type = "prob")
### Predict classes on test set
pred.Mod_RF.1618.OHS.simple <- factor(ifelse(probsTest.Mod_RF.1618.OHS.simple[, "YES"] > 
                                               best_p_threshold.Mod_RF.1618.OHS.simple,
                                             "YES", "NO")) 
pred.Mod_RF.1618.OHS.simple<-relevel(pred.Mod_RF.1618.OHS.simple, "YES")
table(pred.Mod_RF.1618.OHS.simple)
# YES    NO 
# 21501  8158   
head(probsTest.Mod_RF.1618.OHS.simple)
#        YES    NO
# 300492 0.904 0.096
# 300493 0.944 0.056
# 300495 0.950 0.050
# 300496 0.752 0.248
# 300497 0.788 0.212
# 300498 0.826 0.174
head(pred.Mod_RF.1618.OHS.simple) 
# [1] YES YES YES YES YES YES
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_RF.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_RF.1618.OHS.simple,
  reference = English.test.1819.OHS.simple$OHS_MCID,
  positive = "YES")
conf.Matrix.Mod_RF.1618.OHS.simple$table
#              Reference
# Prediction   YES    NO
# YES        20606   895
# NO          6622  1536
### Extract relevant training and testing (English test set) metrics using custom-made function
source("S:/ClinEng_Users/CEDAR/Training & KSF info and forms/STP/STP - AURA/MSc Uni Manchester/Dissertation/code/R/ProjectFUN.R") # custom-made functions
METRICS_Mod_RF.1618.OHS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_RF.1618.OHS.simple,
  trained.model = Mod_RF.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_RF.1618.OHS.simple,
  test_set = English.test.1819.OHS.simple,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_RF.1618.OHS.simple,
  algorithm ="RF_OHS_EngTest"
)
#METRICS_Mod_RF.1618.OHS.simple_EngTest
write.csv(METRICS_Mod_RF.1618.OHS.simple_EngTest,
          "MODELS_METRICS/METRICS_Mod_RF.1618.OHS.simple_EngTest.csv", row.names = FALSE)  
#.........................................................................................
#.........................................................................................

# Test Random Forest on OHS (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_RF.1618.OHS.simple <- predict(Mod_RF.1618.OHS.simple, 
                                            newdata = AMP_HIPS_CLEANED3.test, type = "prob")
### Predict classes on test set
pred.Mod_RF.1618.OHS.simple <- factor(ifelse(probsTest.Mod_RF.1618.OHS.simple[, "YES"] > 
                                               best_p_threshold.Mod_RF.1618.OHS.simple,
                                             "YES", "NO")) 
pred.Mod_RF.1618.OHS.simple<-relevel(pred.Mod_RF.1618.OHS.simple, "YES")
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_RF.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_RF.1618.OHS.simple,
  reference = AMP_HIPS_CLEANED3.test$OHS_MCID,
  positive = "YES")
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_RF.1618.OHS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_RF.1618.OHS.simple,
  trained.model = Mod_RF.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_RF.1618.OHS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_RF.1618.OHS.simple,
  algorithm ="RF_OHS_WelshTest"
)
write.csv(METRICS_Mod_RF.1618.OHS.simple_WelshTest,
          "output/thesis_files//METRICS_Mod_RF.1618.OHS.simple_WelshTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Train Elastic Net on OHS ----------------------------------------
attach("code/02_ML_main_OHS.RData")
English.training.1618.OHS.simple<-English.training.1618.OHS.simple
English.test.1819.OHS.simple<-English.test.1819.OHS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
##Train the model with caret
#### Model OHS SIMPLE: ELASTIC NET
set.seed(1)
Mod_GLMNET.1618.OHS.simple = train(
  form = OHS_MCID ~ .,
  data = English.training.1618.OHS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, ## are these the best values to use for repeated cv?
                           sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "glmnet",
  metric = 'ROC' # I am not sure this actually gets used for glm
)
#Mod_GLMNET.1618.OHS.simple
#summary(Mod_GLMNET.1618.OHS.simple)
source("code/models/OHS_simple/HIPS_ML_training_1618_GLMNET_simple_EngTest.RData")
source("code/models/OHS_simple/HIPS_ML_training_1618_GLMNET_simple_WelshTest.RData")
## selected hyperparameters
head(Mod_GLMNET.1618.OHS.simple$results[
  order(Mod_GLMNET.1618.OHS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_GLMNET.1618.OHS.simple <- thresholder(Mod_GLMNET.1618.OHS.simple,
                                              threshold = seq(0,1,0.05),
                                              final = TRUE,
                                              statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_GLMNET.1618.OHS.simple<-ths.Mod_GLMNET.1618.OHS.simple[ths.Mod_GLMNET.1618.OHS.simple$J == 
                                                                              max(ths.Mod_GLMNET.1618.OHS.simple$J),
                                                                            "prob_threshold"]
best_p_threshold.Mod_GLMNET.1618.OHS.simple ## p = 0.5
### Variable importance
plot(varImp(Mod_GLMNET.1618.OHS.simple), top = 10, 
     main="Variable importance (OHS MCID)\nElastic Net (Simple model)")   
#.........................................................................................
#.........................................................................................

# Test Elastic Net on OHS (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_GLMNET.1618.OHS.simple <- predict(Mod_GLMNET.1618.OHS.simple, 
                                                newdata = English.test.1819.OHS.simple, type = "prob")
### Predict classes on test set
pred.Mod_GLMNET.1618.OHS.simple <- factor(ifelse(probsTest.Mod_GLMNET.1618.OHS.simple[, "YES"] > 
                                                   best_p_threshold.Mod_GLMNET.1618.OHS.simple,
                                                 "YES", "NO")) 
pred.Mod_GLMNET.1618.OHS.simple<-relevel(pred.Mod_GLMNET.1618.OHS.simple, "YES")
table(pred.Mod_GLMNET.1618.OHS.simple)
# YES    NO 
# 21966  7693  
head(probsTest.Mod_GLMNET.1618.OHS.simple)
#   YES        NO
# 1 0.5146583 0.4853417
# 2 0.6227482 0.3772518
# 3 0.6779307 0.3220693
# 4 0.5204576 0.4795424
# 5 0.6240544 0.3759456
# 6 0.6906888 0.3093112
head(pred.Mod_GLMNET.1618.OHS.simple) 
# [1] YES YES YES YES YES YES
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_GLMNET.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_GLMNET.1618.OHS.simple,
  reference = English.test.1819.OHS.simple$OHS_MCID,
  positive = "YES")
conf.Matrix.Mod_GLMNET.1618.OHS.simple$table
#              Reference
# Prediction   YES    NO
# YES        21027   939
# NO          6201  1492
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_GLMNET.1618.OHS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_GLMNET.1618.OHS.simple,
  trained.model = Mod_GLMNET.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_GLMNET.1618.OHS.simple,
  test_set = English.test.1819.OHS.simple,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_GLMNET.1618.OHS.simple,
  algorithm ="GLMNET_OHS_EngTest"
)
#METRICS_Mod_GLMNET.1618.OHS.simple_EngTest
write.csv(METRICS_Mod_GLMNET.1618.OHS.simple_EngTest,
          "MODELS_METRICS/METRICS_Mod_GLMNET.1618.OHS.simple_EngTest.csv", row.names = FALSE)  
#.........................................................................................
#.........................................................................................

# Test Random Forest on OHS (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_GLMNET.1618.OHS.simple <- predict(Mod_GLMNET.1618.OHS.simple, 
                                                newdata = AMP_HIPS_CLEANED3.test, type = "prob")
### Predict classes on test set
pred.Mod_GLMNET.1618.OHS.simple <- factor(ifelse(probsTest.Mod_GLMNET.1618.OHS.simple[, "YES"] > 
                                                   best_p_threshold.Mod_GLMNET.1618.OHS.simple,
                                                 "YES", "NO")) 
pred.Mod_GLMNET.1618.OHS.simple<-relevel(pred.Mod_GLMNET.1618.OHS.simple, "YES")
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_GLMNET.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_GLMNET.1618.OHS.simple,
  reference = AMP_HIPS_CLEANED3.test$OHS_MCID,
  positive = "YES")
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_GLMNET.1618.OHS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_GLMNET.1618.OHS.simple,
  trained.model = Mod_GLMNET.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_GLMNET.1618.OHS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_GLMNET.1618.OHS.simple,
  algorithm ="GLMNET_OHS_WelshTest"
)
write.csv(METRICS_Mod_GLMNET.1618.OHS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_GLMNET.1618.OHS.simple_WelshTest.csv", row.names = FALSE)

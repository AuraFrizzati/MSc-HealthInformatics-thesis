### Import libraries ---------------------------------------------------------------
library(caret) # Machine learning library
library(dplyr)
source("code/00_ProjectFunctions.R") # custom-made functions
#.........................................................................................
#.........................................................................................


### Import input data -------------------------------------------------------
attach("code/01_Preprocess_EnglishHips.RData")
#ls(2)
English.training.1618 <-English.training.1618 ##import English dataset training
English.test.1819 <- English.test.1819 ##import English dataset testing

attach("code/01_Preprocess_WelshHips.RData")
ls(2)
AMP_HIPS_CLEANED_3<-AMP_HIPS_CLEANED_3
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
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED_3[predictors.simple]
### Setting the factor order for outcome variable OHS_MCID for dataset AMP_HIPS_CLEANED3.test
AMP_HIPS_CLEANED3.test[AMP_HIPS_CLEANED3.test$OHS_MCID==1,]$OHS_MCID<-"YES"
AMP_HIPS_CLEANED3.test[AMP_HIPS_CLEANED3.test$OHS_MCID==0,]$OHS_MCID<-"NO"
### Setting the positive outcome to be YES for caret
AMP_HIPS_CLEANED3.test$OHS_MCID <- factor(AMP_HIPS_CLEANED3.test$OHS_MCID, levels = c("YES", "NO"))
#table(AMP_HIPS_CLEANED3.test$OHS_MCID)
#prop.table(table(AMP_HIPS_CLEANED3.test$OHS_MCID))
#nrow(AMP_HIPS_CLEANED3.test) ## 440

rm(English.test.1819,English.training.1618,AMP_HIPS_CLEANED_3, predictors.simple)
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

attach("H:/MSc_thesis_models/NEW/simple_models/HIPS_ML_training_1618_XGBTREE_simple_EngTest.RData")
ls(2)
Mod_XGBTREE.1618.OHS.simple<-Mod_XGBTREE.1618.OHS.simple

Mod_XGBTREE.1618.OHS.simple

## selected hyperparameters
head(Mod_XGBTREE.1618.OHS.simple$results[
  order(Mod_XGBTREE.1618.OHS.simple$results$ROC, decreasing = TRUE),],1)
#    eta max_depth gamma colsample_bytree min_child_weight subsample nrounds       ROC      Sens      Spec
# 32 0.3         2     0              0.8                1      0.75     100 0.7648374 0.7719659 0.6199909
# ROCSD      SensSD     SpecSD
# 32 0.008586026 0.004798505 0.01572285

### [4.2] Define p threshold
ths.Mod_XGBTREE.1618.OHS.simple <- thresholder(Mod_XGBTREE.1618.OHS.simple,
                                               threshold = seq(0,1,0.05),
                                               final = TRUE,
                                               statistics = "all")

#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_XGBTREE.1618.OHS.simple<-ths.Mod_XGBTREE.1618.OHS.simple[ths.Mod_XGBTREE.1618.OHS.simple$J == 
                                                                                max(ths.Mod_XGBTREE.1618.OHS.simple$J),
                                                                              "prob_threshold"]
best_p_threshold.Mod_XGBTREE.1618.OHS.simple ## p = 0.5

### [4.3] variable importance
plot(varImp(Mod_XGBTREE.1618.OHS.simple), top = 10, 
     main="Variable importance (OHS MCID)\nExtreme Gradient Boosting Tree (Simple model)") 
#width = 600, height = 400

### [4.4.1] Predicting probability scores on English test set
probsTest.Mod_XGBTREE.1618.OHS.simple <- predict(Mod_XGBTREE.1618.OHS.simple, 
                                                 newdata = English.test.1819.OHS.simple, type = "prob")

### [4.4.2] calibration curves (caret package)
cal.Mod_XGBTREE.1618.OHS.simple.EngTest<-cbind(English.test.1819.OHS.simple$OHS_MCID,probsTest.Mod_XGBTREE.1618.OHS.simple)
names(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)<-c("OHS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest<-calibration(OHS_MCID ~ YES, data = cal.Mod_XGBTREE.1618.OHS.simple.EngTest)
cal.plot.Mod_XGBTREE.1618.OHS.simple.EngTest<-plot(cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest)
cal.ggplot.Mod_XGBTREE.1618.OHS.simple.EngTest<-ggplot(cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest)

#rm(cal.Mod_XGBTREE.1618.OHS.simple.EngTest,cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest)

## Hosmer-Lemeshow calibration
head(English.test.1819.OHS.simple$OHS_MCID) ##true outcome
head(probsTest.Mod_XGBTREE.1618.OHS.simple)[1] ## predicted score
cal.Mod_XGBTREE.1618.OHS.simple.EngTest<-cbind(English.test.1819.OHS.simple$OHS_MCID,
                                               probsTest.Mod_XGBTREE.1618.OHS.simple[1])
names(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)<-c("True_outcome", "Prob_YES")
head(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)
cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_logic <- ifelse(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome == "YES", TRUE, FALSE)
head(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)
table(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome,cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_logic)
cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_num <- ifelse(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome == "YES", 1, 0)
table(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome,
      cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_num)

# 
# library(DescTools)
# BrierScore(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_logic, cal.Mod_XGBTREE.1618.OHS.simple.EngTest$Prob_YES)
# #[1] 0.1756531

library(PredictABEL)
plotCalibration(data=cal.Mod_XGBTREE.1618.OHS.simple.EngTest, 
                cOutcome=4, 
                predRisk=cal.Mod_XGBTREE.1618.OHS.simple.EngTest$Prob_YES)

head(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)

library(ResourceSelection)
hoslem.test(cal.Mod_XGBTREE.1618.OHS.simple.EngTest$Prob_YES, ## expected/predicted
            cal.Mod_XGBTREE.1618.OHS.simple.EngTest$True_outcome_num) ## observed
#Hosmer and Lemeshow goodness of fit (GOF) test
# X-squared = 39189, df = 8, p-value < 2.2e-16

### [4.4.3] Predict classes on test set
pred.Mod_XGBTREE.1618.OHS.simple <- factor(ifelse(probsTest.Mod_XGBTREE.1618.OHS.simple[, "YES"] > 
                                                    best_p_threshold.Mod_XGBTREE.1618.OHS.simple,
                                                  "YES", "NO")) ##should it be > or >= best_p_threshold?
pred.Mod_XGBTREE.1618.OHS.simple<-relevel(pred.Mod_XGBTREE.1618.OHS.simple, "YES")
table(pred.Mod_XGBTREE.1618.OHS.simple)
# YES    NO 
# 22309  7350   

head(probsTest.Mod_XGBTREE.1618.OHS.simple)
#   YES       NO
# 1 0.6674411 0.3325589
# 2 0.6116700 0.3883300
# 3 0.7340595 0.2659405
# 4 0.5280747 0.4719253
# 5 0.6002516 0.3997484
# 6 0.6386713 0.3613287

head(pred.Mod_XGBTREE.1618.OHS.simple) ##YES is predicted when p > 0.5
# [1] YES YES YES YES YES YES

### [4.4.4] Create confusion matrix for predictions on test set
conf.Matrix.Mod_XGBTREE.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_XGBTREE.1618.OHS.simple,
  reference = English.test.1819.OHS.simple$OHS_MCID,
  positive = "YES")

conf.Matrix.Mod_XGBTREE.1618.OHS.simple$table
#             Reference
# Prediction   YES    NO
# YES        21362   947
# NO          5866  1484

### [4.4.5] Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_XGBTREE.1618.OHS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_XGBTREE.1618.OHS.simple,
  trained.model = Mod_XGBTREE.1618.OHS.simple,
  test_set.model_probabilities = probsTest.Mod_XGBTREE.1618.OHS.simple,
  test_set = English.test.1819.OHS.simple,
  OutputVar = "OHS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_XGBTREE.1618.OHS.simple,
  algorithm ="XGBTREE_OHS_EngTest"
)

METRICS_Mod_XGBTREE.1618.OHS.simple_EngTest
write.csv(METRICS_Mod_XGBTREE.1618.OHS.simple_EngTest,
          "MODELS_METRICS/METRICS_Mod_XGBTREE.1618.OHS.simple_EngTest.csv", row.names = FALSE)

### [4.5.1] Predicting probability scores on Welsh test set
probsTest.Mod_XGBTREE.1618.OHS.simple <- predict(Mod_XGBTREE.1618.OHS.simple, 
                                                 newdata = AMP_HIPS_CLEANED3.test, type = "prob")

### [4.5.2] calibration curves (caret package)
cal.Mod_XGBTREE.1618.OHS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$OHS_MCID,
                                                 probsTest.Mod_XGBTREE.1618.OHS.simple)
names(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)<-c("OHS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest<-calibration(OHS_MCID ~ YES, 
                                                           data = cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)
cal.plot.Mod_XGBTREE.1618.OHS.simple.WelshTest<-plot(cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest)
cal.ggplot.Mod_XGBTREE.1618.OHS.simple.WelshTest<-ggplot(cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest)

#rm(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest, cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest)

## Hosmer-Lemeshow calibration
head(AMP_HIPS_CLEANED3.test$OHS_MCID) ##true outcome
head(probsTest.Mod_XGBTREE.1618.OHS.simple)[1] ## predicted score
cal.Mod_XGBTREE.1618.OHS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$OHS_MCID,
                                                 probsTest.Mod_XGBTREE.1618.OHS.simple[1])
names(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)<-c("True_outcome", "Prob_YES")
head(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)
cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_logic <- ifelse(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome == "YES", TRUE, FALSE)
head(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)
table(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome,cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_logic)
cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_num <- ifelse(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome == "YES", 1, 0)
table(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome,
      cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_num)

# library(DescTools)
# BrierScore(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_logic, cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$Prob_YES)
#[1] 0.1906761

library(PredictABEL)
plotCalibration(data=cal.Mod_XGBTREE.1618.OHS.simple.WelshTest, 
                cOutcome=4, 
                predRisk=cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$Prob_YES)

head(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)

library(ResourceSelection)
hoslem.test(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$Prob_YES, ## expected/predicted
            cal.Mod_XGBTREE.1618.OHS.simple.WelshTest$True_outcome_num) ## observed
#Hosmer and Lemeshow goodness of fit (GOF) test
# X-squared = 405.28, df = 8, p-value < 2.2e-16


### [4.5.3] Predict classes on test set
pred.Mod_XGBTREE.1618.OHS.simple <- factor(ifelse(probsTest.Mod_XGBTREE.1618.OHS.simple[, "YES"] > 
                                                    best_p_threshold.Mod_XGBTREE.1618.OHS.simple,
                                                  "YES", "NO")) ##should it be > or >= best_p_threshold?
pred.Mod_XGBTREE.1618.OHS.simple<-relevel(pred.Mod_XGBTREE.1618.OHS.simple, "YES")
table(pred.Mod_XGBTREE.1618.OHS.simple)
# YES    NO 
# 523   300  

head(probsTest.Mod_XGBTREE.1618.OHS.simple)
#   YES         NO
# 1 0.5091251 0.49087495
# 2 0.6005671 0.39943290
# 3 0.2313250 0.76867503
# 4 0.9152961 0.08470392
# 5 0.8673570 0.13264298
# 6 0.7429310 0.25706899

head(pred.Mod_XGBTREE.1618.OHS.simple) ##NO is predicted when p > 0.45
# [1] YES  YES NO YES YES YES

### [4.5.4] Create confusion matrix for predictions on test set
conf.Matrix.Mod_XGBTREE.1618.OHS.simple<-confusionMatrix(
  data = pred.Mod_XGBTREE.1618.OHS.simple,
  reference = AMP_HIPS_CLEANED3.test$OHS_MCID,
  positive = "YES")


conf.Matrix.Mod_XGBTREE.1618.OHS.simple$table
#            Reference
# Prediction YES  NO
# YES        372 151
# NO         54  246

### [4.5.5] Extract relevant training and testing (Welsh test set) metrics using custom-made function
source("S:/ClinEng_Users/CEDAR/Training & KSF info and forms/STP/STP - AURA/MSc Uni Manchester/Dissertation/code/R/ProjectFUN.R") # custom-made functions
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
          "MODELS_METRICS/OHS-OHS/simple/METRICS_Mod_XGBTREE.1618.OHS.simple_WelshTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................



# Train Logistic Regression on OHS ----------------------------------------
attach("code/02_ML_main_OHS.RData")
English.training.1618.OHS.simple<-English.training.1618.OHS.simple
English.test.1819.OHS.simple<-English.test.1819.OHS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
##Train the model with caret
#library(caret)
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
#attach("code/models/HIPS_ML_training_1618_LR_simple_EngTest.RData")
#attach("code/models/HIPS_ML_training_1618_LR_simple_WelshTest.RData")


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




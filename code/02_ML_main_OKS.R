##### Analysis of OKS MCID - SIMPLE MODEL 18 predictors

### Import libraries ---------------------------------------------------------------
library(caret) # Machine learning library
library(dplyr)
source("code/00_ProjectFunctions.R") # custom-made functions
#.........................................................................................
#.........................................................................................

### Import input data -------------------------------------------------------
attach("code/01_Preprocess_EnglishKnees.RData")
English.training.1618 <-English.training.1618 ##import English dataset training
English.test.1819 <- English.test.1819 ##import English dataset testing
attach("code/01_Preprocess_WelshKnees.RData")
AMP_KNEES_CLEANED3<-AMP_KNEES_CLEANED3
#.........................................................................................
#.........................................................................................

### Pre-processing input data -------------------------------------------------------
### Setting the factor order for outcome variable OKS_MCID
English.training.1618[English.training.1618$OKS_MCID==1,]$OKS_MCID<-"YES"
English.training.1618[English.training.1618$OKS_MCID==0,]$OKS_MCID<-"NO"
### Setting the positive outcome to be YES for caret
English.training.1618$OKS_MCID <- factor(English.training.1618$OKS_MCID, levels = c("YES", "NO"))
#table(English.training.1618$OKS_MCID)
#prop.table(table(English.training.1618$OKS_MCID))
# outcome class imbalance --> to take care of it during model training via upsampling
### Select predictors for simple model OKS MCID
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
                     "OKS_PREOP_PAIN",
                     "OKS_PREOP_NIGHT_PAIN",
                     "OKS_PREOP_WASHING",
                     "OKS_PREOP_TRANSPORT",
                     "OKS_PREOP_WALKING",
                     "OKS_PREOP_STANDING",
                     "OKS_PREOP_LIMPING",
                     "OKS_PREOP_KNEELING",
                     "OKS_PREOP_WORK",
                     "OKS_PREOP_CONFIDENCE",
                     "OKS_PREOP_SHOPPING",
                     "OKS_PREOP_STAIRS",
                     "OKS_PREOP_TOTSCORE",
                     "OKS_MCID"
)
## data to use for training for simple model
English.training.1618.OKS.simple<-English.training.1618[predictors.simple] ##18 predictors
##### English Test set
### Setting the factor order for outcome variable OKS_MCID
English.test.1819[English.test.1819$OKS_MCID==1,]$OKS_MCID<-"YES"
English.test.1819[English.test.1819$OKS_MCID==0,]$OKS_MCID<-"NO"
English.test.1819$OKS_MCID <- factor(English.test.1819$OKS_MCID, levels = c("YES", "NO"))
#table(English.test.1819$OKS_MCID)
prop.table(table(English.test.1819$OKS_MCID))
## data to use for test for simple model
English.test.1819.OKS.simple<-English.test.1819[predictors.simple] ##18 predictors
##### Welsh Test set
AMP_KNEES_CLEANED3.test<-AMP_KNEES_CLEANED3[predictors.simple]
### Setting the factor order for outcome variable OKS_MCID for dataset AMP_KNEES_CLEANED3.test
AMP_KNEES_CLEANED3.test[AMP_KNEES_CLEANED3.test$OKS_MCID==1,]$OKS_MCID<-"YES"
AMP_KNEES_CLEANED3.test[AMP_KNEES_CLEANED3.test$OKS_MCID==0,]$OKS_MCID<-"NO"
### Setting the positive outcome to be YES for caret
AMP_KNEES_CLEANED3.test$OKS_MCID <- factor(AMP_KNEES_CLEANED3.test$OKS_MCID, levels = c("YES", "NO"))
#table(AMP_KNEES_CLEANED3.test$OKS_MCID)
#prop.table(table(AMP_KNEES_CLEANED3.test$OKS_MCID))
#nrow(AMP_KNEES_CLEANED3.test) ## 353
#table(AMP_KNEES_CLEANED3.test$AGEBAND)
#.........................................................................................
#.........................................................................................

# Train XGBoost on OKS ----------------------------------------
attach("code/02_ML_main_OKS.RData")
English.training.1618.OKS.simple<-English.training.1618.OKS.simple
English.test.1819.OKS.simple<-English.test.1819.OKS.simple
AMP_KNEES_CLEANED3.test<-AMP_KNEES_CLEANED3.test
#gc()
#memory.size()
library(caret)
set.seed(1)
Mod_XGBTREE.OKS.simple = train(
  form = OKS_MCID ~ .,
  data = English.training.1618.OKS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE,
                           savePredictions = T, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           verboseIter = TRUE,
                           returnData = FALSE, # to avoid to save the training data inside the model
                           trim = TRUE
  ),
  method = "xgbTree",
  metric = "ROC" 
)
#### saved in 
#load("code/models/OKS_simple/KNEES_ML_training_1618_XGBTREE_simple_EngTest.RData")
#load("code/models/OKS_simple/KNEES_ML_training_1618_XGBTREE_simple_WelshTest.RData")
Mod_XGBTREE.OKS.simple
## selected hyperparameters
#head(Mod_XGBTREE.OKS.simple$results[
#  order(Mod_XGBTREE.OKS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_XGBTREE.1618.OKS.simple <- thresholder(Mod_XGBTREE.OKS.simple,
                                               threshold = seq(0,1,0.05),
                                               final = TRUE,
                                               statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_XGBTREE.1618.OKS.simple<-ths.Mod_XGBTREE.1618.OKS.simple[ths.Mod_XGBTREE.1618.OKS.simple$J == 
                                                                                max(ths.Mod_XGBTREE.1618.OKS.simple$J),
                                                                              "prob_threshold"]
best_p_threshold.Mod_XGBTREE.1618.OKS.simple ## p = 0.5
### variable importance
plot(varImp(Mod_XGBTREE.OKS.simple), top = 10, 
     main="Variable importance (OKS MCID)\nExtreme Gradient Boosting Tree (Simple model)") 
#width = 600, height = 400
#.........................................................................................
#.........................................................................................

# Test XGBoost on OKS (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_XGBTREE.1618.OKS.simple <- predict(Mod_XGBTREE.OKS.simple, 
                                                 newdata = English.test.1819.OKS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_XGBTREE.1618.OKS.simple.EngTest<-cbind(English.test.1819.OKS.simple$OKS_MCID,probsTest.Mod_XGBTREE.1618.OKS.simple)
names(cal.Mod_XGBTREE.1618.OKS.simple.EngTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.OKS.simple.EngTest<-calibration(OKS_MCID ~ YES, data = cal.Mod_XGBTREE.1618.OKS.simple.EngTest)
cal.plot.Mod_XGBTREE.1618.OKS.simple.EngTest<-plot(cal.obj.Mod_XGBTREE.1618.OKS.simple.EngTest)
cal.ggplot.Mod_XGBTREE.1618.OKS.simple.EngTest<-ggplot(cal.obj.Mod_XGBTREE.1618.OKS.simple.EngTest)
rm(cal.Mod_XGBTREE.1618.OKS.simple.EngTest,cal.obj.Mod_XGBTREE.1618.OKS.simple.EngTest)
## Hosmer-Lemeshow calibration
#head(English.test.1819.OKS.simple$OKS_MCID) ##true outcome
#head(probsTest.Mod_XGBTREE.1618.OKS.simple)[1] ## predicted score
cal.Mod_XGBTREE.1618.OKS.simple.EngTest<-cbind(English.test.1819.OKS.simple$OKS_MCID,
                                               probsTest.Mod_XGBTREE.1618.OKS.simple[1])
#names(cal.Mod_XGBTREE.1618.OKS.simple.EngTest)<-c("True_outcome", "Prob_YES")
#head(cal.Mod_XGBTREE.1618.OKS.simple.EngTest)
cal.Mod_XGBTREE.1618.OKS.simple.EngTest$True_outcome_logic <- ifelse(cal.Mod_XGBTREE.1618.OKS.simple.EngTest$True_outcome == "YES", TRUE, FALSE)
#head(cal.Mod_XGBTREE.1618.OKS.simple.EngTest)
#table(cal.Mod_XGBTREE.1618.OKS.simple.EngTest$True_outcome,cal.Mod_XGBTREE.1618.OKS.simple.EngTest$True_outcome_logic)
cal.Mod_XGBTREE.1618.OKS.simple.EngTest$True_outcome_num <- ifelse(cal.Mod_XGBTREE.1618.OKS.simple.EngTest$True_outcome == "YES", 1, 0)
#table(cal.Mod_XGBTREE.1618.OKS.simple.EngTest$True_outcome,
#      cal.Mod_XGBTREE.1618.OKS.simple.EngTest$True_outcome_num)
ResourceSelection::hoslem.test(cal.Mod_XGBTREE.1618.OKS.simple.EngTest$Prob_YES, ## expected/predicted
            cal.Mod_XGBTREE.1618.OKS.simple.EngTest$True_outcome_num) ## observed
#Hosmer and Lemeshow goodness of fit (GOF) test
# X-squared = 27217, df = 8, p-value < 2.2e-16
### Predict classes on test set
pred.Mod_XGBTREE.1618.OKS.simple <- factor(ifelse(probsTest.Mod_XGBTREE.1618.OKS.simple[, "YES"] > 
                                                    best_p_threshold.Mod_XGBTREE.1618.OKS.simple,
                                                  "YES", "NO")) 
pred.Mod_XGBTREE.1618.OKS.simple<-relevel(pred.Mod_XGBTREE.1618.OKS.simple, "YES")
#table(pred.Mod_XGBTREE.1618.OKS.simple)
#head(probsTest.Mod_XGBTREE.1618.OKS.simple)
#head(pred.Mod_XGBTREE.1618.OKS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_XGBTREE.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_XGBTREE.1618.OKS.simple,
  reference = English.test.1819.OKS.simple$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_XGBTREE.1618.OKS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_XGBTREE.1618.OKS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_XGBTREE.1618.OKS.simple,
  trained.model = Mod_XGBTREE.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_XGBTREE.1618.OKS.simple,
  test_set = English.test.1819.OKS.simple,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_XGBTREE.1618.OKS.simple,
  algorithm ="XGBTREE_OKS_EngTest"
)
#METRICS_Mod_XGBTREE.1618.OKS.simple_EngTest
write.csv(METRICS_Mod_XGBTREE.1618.OKS.simple_EngTest,
          "output/thesis_files/METRICS_Mod_XGBTREE.1618.OKS.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test XGBoost on OKS (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_XGBTREE.1618.OKS.simple <- predict(Mod_XGBTREE.OKS.simple, 
                                                 newdata = AMP_KNEES_CLEANED3.test, type = "prob")
###  calibration curves (caret package)
cal.Mod_XGBTREE.1618.OKS.simple.WelshTest<-cbind(AMP_KNEES_CLEANED3.test$OKS_MCID,
                                                 probsTest.Mod_XGBTREE.1618.OKS.simple)
names(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.OKS.simple.WelshTest<-calibration(OKS_MCID ~ YES, 
                                                           data = cal.Mod_XGBTREE.1618.OKS.simple.WelshTest)
cal.plot.Mod_XGBTREE.1618.OKS.simple.WelshTest<-plot(cal.obj.Mod_XGBTREE.1618.OKS.simple.WelshTest)
cal.ggplot.Mod_XGBTREE.1618.OKS.simple.WelshTest<-ggplot(cal.obj.Mod_XGBTREE.1618.OKS.simple.WelshTest)
rm(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest, cal.obj.Mod_XGBTREE.1618.OKS.simple.WelshTest)
## Hosmer-Lemeshow calibration
#head(AMP_KNEES_CLEANED3.test$OKS_MCID) ##true outcome
#head(probsTest.Mod_XGBTREE.1618.OKS.simple)[1] ## predicted score
cal.Mod_XGBTREE.1618.OKS.simple.WelshTest<-cbind(AMP_KNEES_CLEANED3.test$OKS_MCID,
                                                 probsTest.Mod_XGBTREE.1618.OKS.simple[1])
names(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest)<-c("True_outcome", "Prob_YES")
#head(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest)
cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$True_outcome_logic <- ifelse(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$True_outcome == "YES", TRUE, FALSE)
#head(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest)
#table(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$True_outcome,cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$True_outcome_logic)
cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$True_outcome_num <- ifelse(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$True_outcome == "YES", 1, 0)
#table(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$True_outcome,
#      cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$True_outcome_num)
ResourceSelection::hoslem.test(cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$Prob_YES, ## expected/predicted
            cal.Mod_XGBTREE.1618.OKS.simple.WelshTest$True_outcome_num) ## observed
#Hosmer and Lemeshow goodness of fit (GOF) test
# X-squared = 98.187, df = 8, p-value < 2.2e-16
### Predict classes on test set
pred.Mod_XGBTREE.1618.OKS.simple <- factor(ifelse(probsTest.Mod_XGBTREE.1618.OKS.simple[, "YES"] > 
                                                    best_p_threshold.Mod_XGBTREE.1618.OKS.simple,
                                                  "YES", "NO")) ##should it be > or >= best_p_threshold?
pred.Mod_XGBTREE.1618.OKS.simple<-relevel(pred.Mod_XGBTREE.1618.OKS.simple, "YES")
#table(pred.Mod_XGBTREE.1618.OKS.simple)
#head(probsTest.Mod_XGBTREE.1618.OKS.simple)
#head(pred.Mod_XGBTREE.1618.OKS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_XGBTREE.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_XGBTREE.1618.OKS.simple,
  reference = AMP_KNEES_CLEANED3.test$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_XGBTREE.1618.OKS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_XGBTREE.1618.OKS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_XGBTREE.1618.OKS.simple,
  trained.model = Mod_XGBTREE.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_XGBTREE.1618.OKS.simple,
  test_set = AMP_KNEES_CLEANED3.test,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_XGBTREE.1618.OKS.simple,
  algorithm ="XGBTREE_OKS_WelshTest"
)
#METRICS_Mod_XGBTREE.1618.OKS.simple_WelshTest
write.csv(METRICS_Mod_XGBTREE.1618.OKS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_XGBTREE.1618.OKS.simple_WelshTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Train Logistic Regression on OKS ----------------------------------------
attach("code/02_ML_main_OKS.RData")
English.training.1618.OKS.simple<-English.training.1618.OKS.simple
English.test.1819.OKS.simple<-English.test.1819.OKS.simple
AMP_KNEES_CLEANED3.test<-AMP_KNEES_CLEANED3.test
#gc()
#memory.size()
set.seed(1)
Mod_LR.1618.OKS.simple = train(
  form = OKS_MCID ~ .,
  data = English.training.1618.OKS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3,
                           sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "glm",
  family = "binomial",
  metric = 'ROC' 
)
#### saved in 
#load("code/models/OKS_simple/KNEES_ML_training_1618_LR_simple_EngTest.RData")
#load("code/models/OKS_simple/KNEES_ML_training_1618_LR_simple_WelshTest.RData")
Mod_LR.1618.OKS.simple
#summary(Mod_LR.1618.OKS.simple)
### Define p threshold
ths.Mod_LR.1618.OKS.simple <- thresholder(Mod_LR.1618.OKS.simple,
                                          threshold = seq(0,1,0.05),
                                          final = TRUE,
                                          statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_LR.1618.OKS.simple<-ths.Mod_LR.1618.OKS.simple[ths.Mod_LR.1618.OKS.simple$J == 
                                                                      max(ths.Mod_LR.1618.OKS.simple$J),
                                                                    "prob_threshold"]
best_p_threshold.Mod_LR.1618.OKS.simple ## p = 0.5
### variable importance
plot(varImp(Mod_LR.1618.OKS.simple), top = 10, 
     main="Variable importance (OKS MCID)\nLog Reg (Simple model)") 
#.........................................................................................
#.........................................................................................

# Test Logistic Regression on OKS (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_LR.1618.OKS.simple <- predict(Mod_LR.1618.OKS.simple, 
                                            newdata = English.test.1819.OKS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_LR.1618.OKS.simple.EngTest<-cbind(English.test.1819.OKS.simple$OKS_MCID,
                                          probsTest.Mod_LR.1618.OKS.simple)
names(cal.Mod_LR.1618.OKS.simple.EngTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_LR.1618.OKS.simple.EngTest<-calibration(OKS_MCID ~ YES, 
                                                    data = cal.Mod_LR.1618.OKS.simple.EngTest)
cal.plot.Mod_LR.1618.OKS.simple.EngTest<-plot(cal.obj.Mod_LR.1618.OKS.simple.EngTest)
cal.ggplot.Mod_LR.1618.OKS.simple.EngTest<-ggplot(cal.obj.Mod_LR.1618.OKS.simple.EngTest)
### Predict classes on test set
pred.Mod_LR.1618.OKS.simple <- factor(ifelse(probsTest.Mod_LR.1618.OKS.simple[, "YES"] > 
                                               best_p_threshold.Mod_LR.1618.OKS.simple,
                                             "YES", "NO")) 
pred.Mod_LR.1618.OKS.simple<-relevel(pred.Mod_LR.1618.OKS.simple, "YES")
#table(pred.Mod_LR.1618.OKS.simple)
#head(probsTest.Mod_LR.1618.OKS.simple)
#head(pred.Mod_LR.1618.OKS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_LR.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_LR.1618.OKS.simple,
  reference = English.test.1819.OKS.simple$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_LR.1618.OKS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_LR.1618.OKS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_LR.1618.OKS.simple,
  trained.model = Mod_LR.1618.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_LR.1618.OKS.simple,
  test_set = English.test.1819.OKS.simple,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_LR.1618.OKS.simple,
  algorithm ="LOGREG_OKS_EngTest"
)
#METRICS_Mod_LR.1618.OKS.simple_EngTest
write.csv(METRICS_Mod_LR.1618.OKS.simple_EngTest,
          "output/thesis_files/METRICS_Mod_LR.1618.OKS.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test Logistic Regression on OKS (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_LR.1618.OKS.simple <- predict(Mod_LR.1618.OKS.simple, 
                                            newdata = AMP_KNEES_CLEANED3.test, type = "prob")
### calibration curves (caret package)
cal.Mod_LR.1618.OKS.simple.WelshTest<-cbind(AMP_KNEES_CLEANED3.test$OKS_MCID,
                                            probsTest.Mod_LR.1618.OKS.simple)
names(cal.Mod_LR.1618.OKS.simple.WelshTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_LR.1618.OKS.simple.WelshTest<-calibration(OKS_MCID ~ YES, 
                                                      data = cal.Mod_LR.1618.OKS.simple.WelshTest)
cal.plot.Mod_LR.1618.OKS.simple.WelshTest<-plot(cal.obj.Mod_LR.1618.OKS.simple.WelshTest)
cal.ggplot.Mod_LR.1618.OKS.simple.WelshTest<-ggplot(cal.obj.Mod_LR.1618.OKS.simple.WelshTest)
rm(cal.Mod_LR.1618.OKS.simple.WelshTest,cal.obj.Mod_LR.1618.OKS.simple.WelshTest)
### Predict classes on test set
pred.Mod_LR.1618.OKS.simple <- factor(ifelse(probsTest.Mod_LR.1618.OKS.simple[, "YES"] > 
                                               best_p_threshold.Mod_LR.1618.OKS.simple,
                                             "YES", "NO")) 
pred.Mod_LR.1618.OKS.simple<-relevel(pred.Mod_LR.1618.OKS.simple, "YES")
#table(pred.Mod_LR.1618.OKS.simple)
#head(probsTest.Mod_LR.1618.OKS.simple)
#head(pred.Mod_LR.1618.OKS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_LR.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_LR.1618.OKS.simple,
  reference = AMP_KNEES_CLEANED3.test$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_LR.1618.OKS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_LR.1618.OKS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_LR.1618.OKS.simple,
  trained.model = Mod_LR.1618.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_LR.1618.OKS.simple,
  test_set = AMP_KNEES_CLEANED3.test,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_LR.1618.OKS.simple,
  algorithm ="LOGREG_OKS_WelshTest"
)
write.csv(METRICS_Mod_LR.1618.OKS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_LR.1618.OKS.simple_WelshTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Train Neural Net on OKS ----------------------------------------
attach("code/02_ML_main_OKS.RData")
English.training.1618.OKS.simple<-English.training.1618.OKS.simple
English.test.1819.OKS.simple<-English.test.1819.OKS.simple
AMP_KNEES_CLEANED3.test<-AMP_KNEES_CLEANED3.test
#gc()
#memory.size()
library(caret)
set.seed(1)
Mod_NNET.1618.OKS.simple = train(
  form = OKS_MCID ~ .,
  data = English.training.1618.OKS.simple,
  #x = training.OKS.min_matr,
  #y = outcome,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           sampling = "up", ## outcome class is unbalanced, so up-sampling is required!
                           classProbs = TRUE,
                           savePredictions = T, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           verboseIter = TRUE,
                           returnData = FALSE, # to avoid to save the training data inside the model
                           trim = TRUE
  ),
  method = "nnet",
  metric = "ROC"
)
#### saved in 
#load("code/models/OKS_simple/KNEES_ML_training_1618_NNET_simple_EngTest.RData")
#load("code/models/OKS_simple/KNEES_ML_training_1618_NNET_simple_WelshTest.RData")
Mod_NNET.1618.OKS.simple
## selected hyperparameters
#head(Mod_NNET.1618.OKS.simple$results[
#  order(Mod_NNET.1618.OKS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_NNET.1618.OKS.simple <- thresholder(Mod_NNET.1618.OKS.simple,
                                            threshold = seq(0,1,0.05),
                                            final = TRUE,
                                            statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_NNET.1618.OKS.simple<-ths.Mod_NNET.1618.OKS.simple[ths.Mod_NNET.1618.OKS.simple$J == 
                                                                          max(ths.Mod_NNET.1618.OKS.simple$J),
                                                                        "prob_threshold"]
best_p_threshold.Mod_NNET.1618.OKS.simple ## p = 0.5
### variable importance
plot(varImp(Mod_NNET.1618.OKS.simple), top = 10, 
     main="Variable importance (OKS MCID)\nNeural Net (Simple model)") 
#.........................................................................................
#.........................................................................................

# Test Neural Net on OKS (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_NNET.1618.OKS.simple <- predict(Mod_NNET.1618.OKS.simple, 
                                              newdata = English.test.1819.OKS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_NNET.1618.OKS.simple.EngTest<-cbind(English.test.1819.OKS.simple$OKS_MCID,
                                            probsTest.Mod_NNET.1618.OKS.simple)
names(cal.Mod_NNET.1618.OKS.simple.EngTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_NNET.1618.OKS.simple.EngTest<-calibration(OKS_MCID ~ YES, 
                                                      data = cal.Mod_NNET.1618.OKS.simple.EngTest)
cal.plot.Mod_NNET.1618.OKS.simple.EngTest<-plot(cal.obj.Mod_NNET.1618.OKS.simple.EngTest)
cal.ggplot.Mod_NNET.1618.OKS.simple.EngTest<-ggplot(cal.obj.Mod_NNET.1618.OKS.simple.EngTest)
rm(cal.Mod_NNET.1618.OKS.simple.EngTest,cal.obj.Mod_NNET.1618.OKS.simple.EngTest)
### Predict classes on test set
pred.Mod_NNET.1618.OKS.simple <- factor(ifelse(probsTest.Mod_NNET.1618.OKS.simple[, "YES"] > 
                                                 best_p_threshold.Mod_NNET.1618.OKS.simple,
                                               "YES", "NO")) 
pred.Mod_NNET.1618.OKS.simple<-relevel(pred.Mod_NNET.1618.OKS.simple, "YES")
#table(pred.Mod_NNET.1618.OKS.simple)
#head(probsTest.Mod_NNET.1618.OKS.simple)
#head(pred.Mod_NNET.1618.OKS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_NNET.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_NNET.1618.OKS.simple,
  reference = English.test.1819.OKS.simple$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_NNET.1618.OKS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_NNET.1618.OKS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_NNET.1618.OKS.simple,
  trained.model = Mod_NNET.1618.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_NNET.1618.OKS.simple,
  test_set = English.test.1819.OKS.simple,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_NNET.1618.OKS.simple,
  algorithm ="NNET_OKS_EngTest"
)
#METRICS_Mod_NNET.1618.OKS.simple_EngTest
write.csv(METRICS_Mod_NNET.1618.OKS.simple_EngTest,
          "output/thesis_files/METRICS_Mod_NNET.1618.OKS.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test Neural Net on OKS (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_NNET.1618.OKS.simple <- predict(Mod_NNET.1618.OKS.simple, 
                                              newdata = AMP_KNEES_CLEANED3.test, type = "prob")
### calibration curves (caret package)
cal.Mod_NNET.1618.OKS.simple.WelshTest<-cbind(AMP_KNEES_CLEANED3.test$OKS_MCID,
                                              probsTest.Mod_NNET.1618.OKS.simple)
names(cal.Mod_NNET.1618.OKS.simple.WelshTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_NNET.1618.OKS.simple.WelshTest<-calibration(OKS_MCID ~ YES, 
                                                        data = cal.Mod_NNET.1618.OKS.simple.WelshTest)
cal.plot.Mod_NNET.1618.OKS.simple.WelshTest<-plot(cal.obj.Mod_NNET.1618.OKS.simple.WelshTest)
cal.ggplot.Mod_NNET.1618.OKS.simple.WelshTest<-ggplot(cal.obj.Mod_NNET.1618.OKS.simple.WelshTest)
rm(cal.Mod_NNET.1618.OKS.simple.WelshTest,cal.obj.Mod_NNET.1618.OKS.simple.WelshTest)
### Predict classes on test set
pred.Mod_NNET.1618.OKS.simple <- factor(ifelse(probsTest.Mod_NNET.1618.OKS.simple[, "YES"] > 
                                                 best_p_threshold.Mod_NNET.1618.OKS.simple,
                                               "YES", "NO")) 
pred.Mod_NNET.1618.OKS.simple<-relevel(pred.Mod_NNET.1618.OKS.simple, "YES")
#table(pred.Mod_NNET.1618.OKS.simple)
#head(probsTest.Mod_NNET.1618.OKS.simple)
#head(pred.Mod_NNET.1618.OKS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_NNET.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_NNET.1618.OKS.simple,
  reference = AMP_KNEES_CLEANED3.test$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_NNET.1618.OKS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_NNET.1618.OKS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_NNET.1618.OKS.simple,
  trained.model = Mod_NNET.1618.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_NNET.1618.OKS.simple,
  test_set = AMP_KNEES_CLEANED3.test,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_NNET.1618.OKS.simple,
  algorithm ="NNET_OKS_WelshTest"
)
write.csv(METRICS_Mod_NNET.1618.OKS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_NNET.1618.OKS.simple_WelshTest.csv", row.names = FALSE) 
#.........................................................................................
#.........................................................................................

# Train Random Forest on OKS ----------------------------------------
attach("code/02_ML_main_OKS.RData")
English.training.1618.OKS.simple<-English.training.1618.OKS.simple
English.test.1819.OKS.simple<-English.test.1819.OKS.simple
AMP_KNEES_CLEANED3.test<-AMP_KNEES_CLEANED3.test
#gc()
#memory.size()
library(caret)
set.seed(1)
Mod_RF.1618.OKS.simple = train(
  form = OKS_MCID ~ .,
  data = English.training.1618.OKS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE,
                           returnData = FALSE, # to avoid to save the training data inside the model
                           trim = TRUE), 
  method = "rf",
  metric = 'ROC' 
)
#### saved in 
#load("code/models/OKS_simple/KNEES_ML_training_1618_RF_simple_EngTest.RData")
#load("code/models/OKS_simple/KNEES_ML_training_1618_RF_simple_WelshTest.RData")
Mod_RF.1618.OKS.simple
## selected hyperparameters
#head(Mod_RF.1618.OKS.simple$results[
#  order(Mod_RF.1618.OKS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_RF.1618.OKS.simple <- thresholder(Mod_RF.1618.OKS.simple,
                                          threshold = seq(0,1,0.05),
                                          final = TRUE,
                                          statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_RF.1618.OKS.simple<-ths.Mod_RF.1618.OKS.simple[ths.Mod_RF.1618.OKS.simple$J == 
                                                                      max(ths.Mod_RF.1618.OKS.simple$J),
                                                                    "prob_threshold"]
best_p_threshold.Mod_RF.1618.OKS.simple ## p = 0.7
### variable importance
plot(varImp(Mod_RF.1618.OKS.simple), top = 10, 
     main="Variable importance (OKS MCID)\nRandom Forest (Simple model)")
#.........................................................................................
#.........................................................................................

# Test Random Forest on OKS (English Test set)---------------------------------------- 
### Predicting probability scores on English test set
probsTest.Mod_RF.1618.OKS.simple <- predict(Mod_RF.1618.OKS.simple, 
                                            newdata = English.test.1819.OKS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_RF.1618.OKS.simple.EngTest<-cbind(English.test.1819.OKS.simple$OKS_MCID,
                                          probsTest.Mod_RF.1618.OKS.simple)
names(cal.Mod_RF.1618.OKS.simple.EngTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_RF.1618.OKS.simple.EngTest<-calibration(OKS_MCID ~ YES, 
                                                    data = cal.Mod_RF.1618.OKS.simple.EngTest)
cal.plot.Mod_RF.1618.OKS.simple.EngTest<-plot(cal.obj.Mod_RF.1618.OKS.simple.EngTest)
cal.ggplot.Mod_RF.1618.OKS.simple.EngTest<-ggplot(cal.obj.Mod_RF.1618.OKS.simple.EngTest)
rm(cal.Mod_RF.1618.OKS.simple.EngTest,cal.obj.Mod_RF.1618.OKS.simple.EngTest)
### Predict classes on test set
pred.Mod_RF.1618.OKS.simple <- factor(ifelse(probsTest.Mod_RF.1618.OKS.simple[, "YES"] > 
                                               best_p_threshold.Mod_RF.1618.OKS.simple,
                                             "YES", "NO")) 
pred.Mod_RF.1618.OKS.simple<-relevel(pred.Mod_RF.1618.OKS.simple, "YES")
#table(pred.Mod_RF.1618.OKS.simple)
#head(probsTest.Mod_RF.1618.OKS.simple)
#head(pred.Mod_RF.1618.OKS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_RF.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_RF.1618.OKS.simple,
  reference = English.test.1819.OKS.simple$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_RF.1618.OKS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_RF.1618.OKS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_RF.1618.OKS.simple,
  trained.model = Mod_RF.1618.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_RF.1618.OKS.simple,
  test_set = English.test.1819.OKS.simple,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_RF.1618.OKS.simple,
  algorithm ="RF_OKS_EngTest"
)
#METRICS_Mod_RF.1618.OKS.simple_EngTest
write.csv(METRICS_Mod_RF.1618.OKS.simple_EngTest,
          "output/thesis_files/METRICS_Mod_RF.1618.OKS.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test Random Forest on OKS (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_RF.1618.OKS.simple <- predict(Mod_RF.1618.OKS.simple, 
                                            newdata = AMP_KNEES_CLEANED3.test, type = "prob")
### [6.5.2] calibration curves (caret package)
cal.Mod_RF.1618.OKS.simple.WelshTest<-cbind(AMP_KNEES_CLEANED3.test$OKS_MCID,
                                            probsTest.Mod_RF.1618.OKS.simple)
names(cal.Mod_RF.1618.OKS.simple.WelshTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_RF.1618.OKS.simple.WelshTest<-calibration(OKS_MCID ~ YES, 
                                                      data = cal.Mod_RF.1618.OKS.simple.WelshTest)
cal.plot.Mod_RF.1618.OKS.simple.WelshTest<-plot(cal.obj.Mod_RF.1618.OKS.simple.WelshTest)
cal.ggplot.Mod_RF.1618.OKS.simple.WelshTest<-ggplot(cal.obj.Mod_RF.1618.OKS.simple.WelshTest)
rm(cal.Mod_RF.1618.OKS.simple.WelshTest,cal.obj.Mod_RF.1618.OKS.simple.WelshTest)
### Predict classes on test set
pred.Mod_RF.1618.OKS.simple <- factor(ifelse(probsTest.Mod_RF.1618.OKS.simple[, "YES"] > 
                                               best_p_threshold.Mod_RF.1618.OKS.simple,
                                             "YES", "NO")) 
pred.Mod_RF.1618.OKS.simple<-relevel(pred.Mod_RF.1618.OKS.simple, "YES")
#table(pred.Mod_RF.1618.OKS.simple)
#head(probsTest.Mod_RF.1618.OKS.simple)
#head(pred.Mod_RF.1618.OKS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_RF.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_RF.1618.OKS.simple,
  reference = AMP_KNEES_CLEANED3.test$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_RF.1618.OKS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_RF.1618.OKS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_RF.1618.OKS.simple,
  trained.model = Mod_RF.1618.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_RF.1618.OKS.simple,
  test_set = AMP_KNEES_CLEANED3.test,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_RF.1618.OKS.simple,
  algorithm ="RF_OKS_WelshTest"
)
write.csv(METRICS_Mod_RF.1618.OKS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_RF.1618.OKS.simple_WelshTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Train Elastic Net on OKS ----------------------------------------
attach("code/02_ML_main_OKS.RData")
English.training.1618.OKS.simple<-English.training.1618.OKS.simple
English.test.1819.OKS.simple<-English.test.1819.OKS.simple
AMP_KNEES_CLEANED3.test<-AMP_KNEES_CLEANED3.test
#gc()
#memory.size()
set.seed(1)
Mod_GLMNET.1618.OKS.simple = train(
  form = OKS_MCID ~ .,
  data = English.training.1618.OKS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "glmnet",
  metric = 'ROC' 
)
#### saved in 
#load("code/models/OKS_simple/KNEES_ML_training_1618_GLMNET_simple_EngTest.RData")
#load("code/models/OKS_simple/KNEES_ML_training_1618_GLMNET_simple_WelshTest.RData")
Mod_GLMNET.1618.OKS.simple
#summary(Mod_GLMNET.1618.OKS.simple)
## selected hyperparameters
#head(Mod_GLMNET.1618.OKS.simple$results[
#  order(Mod_GLMNET.1618.OKS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_GLMNET.1618.OKS.simple <- thresholder(Mod_GLMNET.1618.OKS.simple,
                                              threshold = seq(0,1,0.05),
                                              final = TRUE,
                                              statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_GLMNET.1618.OKS.simple<-ths.Mod_GLMNET.1618.OKS.simple[ths.Mod_GLMNET.1618.OKS.simple$J == 
                                                                              max(ths.Mod_GLMNET.1618.OKS.simple$J),
                                                                            "prob_threshold"]
best_p_threshold.Mod_GLMNET.1618.OKS.simple ## p = 0.5
### variable importance
plot(varImp(Mod_GLMNET.1618.OKS.simple), top = 10, 
     main="Variable importance (OKS MCID)\nElastic Net (Simple model)")  
#.........................................................................................
#.........................................................................................

# Test Elastic Net on OKS (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_GLMNET.1618.OKS.simple <- predict(Mod_GLMNET.1618.OKS.simple, 
                                                newdata = English.test.1819.OKS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_GLMNET.1618.OKS.simple.EngTest<-cbind(English.test.1819.OKS.simple$OKS_MCID,
                                              probsTest.Mod_GLMNET.1618.OKS.simple)
names(cal.Mod_GLMNET.1618.OKS.simple.EngTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_GLMNET.1618.OKS.simple.EngTest<-calibration(OKS_MCID ~ YES, 
                                                        data = cal.Mod_GLMNET.1618.OKS.simple.EngTest)
cal.plot.Mod_GLMNET.1618.OKS.simple.EngTest<-plot(cal.obj.Mod_GLMNET.1618.OKS.simple.EngTest)
cal.ggplot.Mod_GLMNET.1618.OKS.simple.EngTest<-ggplot(cal.obj.Mod_GLMNET.1618.OKS.simple.EngTest)
rm(cal.Mod_GLMNET.1618.OKS.simple.EngTest,cal.obj.Mod_GLMNET.1618.OKS.simple.EngTest)
### Predict classes on test set
pred.Mod_GLMNET.1618.OKS.simple <- factor(ifelse(probsTest.Mod_GLMNET.1618.OKS.simple[, "YES"] > 
                                                   best_p_threshold.Mod_GLMNET.1618.OKS.simple,
                                                 "YES", "NO")) 
pred.Mod_GLMNET.1618.OKS.simple<-relevel(pred.Mod_GLMNET.1618.OKS.simple, "YES")
#table(pred.Mod_GLMNET.1618.OKS.simple)
#head(probsTest.Mod_GLMNET.1618.OKS.simple)
#head(pred.Mod_GLMNET.1618.OKS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_GLMNET.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_GLMNET.1618.OKS.simple,
  reference = English.test.1819.OKS.simple$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_GLMNET.1618.OKS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_GLMNET.1618.OKS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_GLMNET.1618.OKS.simple,
  trained.model = Mod_GLMNET.1618.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_GLMNET.1618.OKS.simple,
  test_set = English.test.1819.OKS.simple,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_GLMNET.1618.OKS.simple,
  algorithm ="GLMNET_OKS_EngTest"
)
#METRICS_Mod_GLMNET.1618.OKS.simple_EngTest
write.csv(METRICS_Mod_GLMNET.1618.OKS.simple_EngTest,
          "output/thesis_files/METRICS_Mod_GLMNET.1618.OKS.simple_EngTest.csv", row.names = FALSE) 
#.........................................................................................
#.........................................................................................

# Test Random Forest on OKS (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_GLMNET.1618.OKS.simple <- predict(Mod_GLMNET.1618.OKS.simple, 
                                                newdata = AMP_KNEES_CLEANED3.test, type = "prob")
### calibration curves (caret package)
cal.Mod_GLMNET.1618.OKS.simple.WelshTest<-cbind(AMP_KNEES_CLEANED3.test$OKS_MCID,
                                                probsTest.Mod_GLMNET.1618.OKS.simple)
names(cal.Mod_GLMNET.1618.OKS.simple.WelshTest)<-c("OKS_MCID","YES","NO")
cal.obj.Mod_GLMNET.1618.OKS.simple.WelshTest<-calibration(OKS_MCID ~ YES, 
                                                          data = cal.Mod_GLMNET.1618.OKS.simple.WelshTest)
cal.plot.Mod_GLMNET.1618.OKS.simple.WelshTest<-plot(cal.obj.Mod_GLMNET.1618.OKS.simple.WelshTest)
cal.ggplot.Mod_GLMNET.1618.OKS.simple.WelshTest<-ggplot(cal.obj.Mod_GLMNET.1618.OKS.simple.WelshTest)
rm(cal.Mod_GLMNET.1618.OKS.simple.WelshTest,cal.obj.Mod_GLMNET.1618.OKS.simple.WelshTest)
### Predict classes on test set
pred.Mod_GLMNET.1618.OKS.simple <- factor(ifelse(probsTest.Mod_GLMNET.1618.OKS.simple[, "YES"] > 
                                                   best_p_threshold.Mod_GLMNET.1618.OKS.simple,
                                                 "YES", "NO")) 
pred.Mod_GLMNET.1618.OKS.simple<-relevel(pred.Mod_GLMNET.1618.OKS.simple, "YES")
#table(pred.Mod_GLMNET.1618.OKS.simple)
#head(probsTest.Mod_GLMNET.1618.OKS.simple)
#head(pred.Mod_GLMNET.1618.OKS.simple) ##NO is predicted when p > 0.45
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_GLMNET.1618.OKS.simple<-confusionMatrix(
  data = pred.Mod_GLMNET.1618.OKS.simple,
  reference = AMP_KNEES_CLEANED3.test$OKS_MCID,
  positive = "YES")
#conf.Matrix.Mod_GLMNET.1618.OKS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_GLMNET.1618.OKS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_GLMNET.1618.OKS.simple,
  trained.model = Mod_GLMNET.1618.OKS.simple,
  test_set.model_probabilities = probsTest.Mod_GLMNET.1618.OKS.simple,
  test_set = AMP_KNEES_CLEANED3.test,
  OutputVar = "OKS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_GLMNET.1618.OKS.simple,
  algorithm ="GLMNET_OKS_WelshTest"
)
write.csv(METRICS_Mod_GLMNET.1618.OKS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_GLMNET.1618.OKS.simple_WelshTest.csv", row.names = FALSE)












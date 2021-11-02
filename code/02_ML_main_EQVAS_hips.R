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
AMP_HIPS_CLEANED3<-AMP_HIPS_CLEANED_3
#.........................................................................................
#.........................................................................................

### Pre-processing input data -------------------------------------------------------
### Setting the factor order for outcome variable VAS_MCID
English.training.1618[English.training.1618$VAS_MCID==1,]$VAS_MCID<-"YES"
English.training.1618[English.training.1618$VAS_MCID==0,]$VAS_MCID<-"NO"
### Setting the positive outcome to be YES for caret
English.training.1618$VAS_MCID <- factor(English.training.1618$VAS_MCID, levels = c("YES", "NO"))
## --> predicting the probability of not achieving PostOp VAS MCID
#table(English.training.1618$VAS_MCID)
#prop.table(table(English.training.1618$VAS_MCID))
# NO outcome class imbalance
### Select predictors for simple model VAS MCID
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
                     "VAS_MCID"
)
## data to use for training for simple model
English.training.1618.VAS.simple<-English.training.1618[predictors.simple] ##18 predictors
### Setting the factor order for outcome variable VAS_MCID
English.test.1819[English.test.1819$VAS_MCID==1,]$VAS_MCID<-"YES"
English.test.1819[English.test.1819$VAS_MCID==0,]$VAS_MCID<-"NO"
### The reference level needs to be set as YES to have the probability of predicting NO
English.test.1819$VAS_MCID <- factor(English.test.1819$VAS_MCID, levels = c("YES", "NO"))
## --> predicting the probability of not achieving PostOp VAS MCID
#table(English.test.1819$VAS_MCID)
#prop.table(table(English.test.1819$VAS_MCID))
## data to use for test for simple model
English.test.1819.VAS.simple<-English.test.1819[predictors.simple] ##18 predictors
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3[predictors.simple]
### Setting the factor order for outcome variable VAS_MCID for dataset AMP_HIPS_CLEANED3.test
AMP_HIPS_CLEANED3.test[AMP_HIPS_CLEANED3.test$VAS_MCID==1,]$VAS_MCID<-"YES"
AMP_HIPS_CLEANED3.test[AMP_HIPS_CLEANED3.test$VAS_MCID==0,]$VAS_MCID<-"NO"
### Setting the positive outcome to be YES for caret
AMP_HIPS_CLEANED3.test$VAS_MCID <- factor(AMP_HIPS_CLEANED3.test$VAS_MCID, levels = c("YES", "NO"))
## --> predicting the probability of not achieving PostOp VAS MCID
#table(AMP_HIPS_CLEANED3.test$VAS_MCID)
#prop.table(table(AMP_HIPS_CLEANED3.test$VAS_MCID))
#.........................................................................................
#.........................................................................................

# Train XGBoost on EQ-VAS (hips) ----------------------------------------
attach("code/02_ML_main_EQVAS_hips.RData")
English.training.1618.VAS.simple<-English.training.1618.VAS.simple
English.test.1819.VAS.simple<-English.test.1819.VAS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
#gc()
#memory.size()
library(caret)
set.seed(1)
Mod_XGBTREE.1618.VAS.simple = train(
  form = VAS_MCID ~ .,
  data = English.training.1618.VAS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, ## are these the best values to use for repeated cv?
                           ###sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "xgbTree",
  metric = 'ROC'
)
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_XGBTREE_VAS_simple_EngTest.RData")
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_XGBTREE_VAS_simple_WelshTest.RData")
Mod_XGBTREE.1618.VAS.simple<-Mod_XGBTREE.1618.VAS.simple
Mod_XGBTREE.1618.VAS.simple
## selected hyperparameters
head(Mod_XGBTREE.1618.VAS.simple$results[
  order(Mod_XGBTREE.1618.VAS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_XGBTREE.1618.VAS.simple <- thresholder(Mod_XGBTREE.1618.VAS.simple,
                                               threshold = seq(0,1,0.05),
                                               final = TRUE,
                                               statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_XGBTREE.1618.VAS.simple<-ths.Mod_XGBTREE.1618.VAS.simple[ths.Mod_XGBTREE.1618.VAS.simple$J == 
                                                                                max(ths.Mod_XGBTREE.1618.VAS.simple$J),
                                                                              "prob_threshold"]
best_p_threshold.Mod_XGBTREE.1618.VAS.simple ## p = 0.45

### variable importance
plot(varImp(Mod_XGBTREE.1618.VAS.simple), top = 10, 
     main="Variable importance (VAS MCID)\nExtreme Gradient Boosting Tree (Simple model)") 
#width = 600, height = 400
#.........................................................................................
#.........................................................................................

# Test XGBoost on EQ-VAS hips (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_XGBTREE.1618.VAS.simple <- predict(Mod_XGBTREE.1618.VAS.simple, 
                                                 newdata = English.test.1819.VAS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_XGBTREE.1618.VAS.simple.EngTest<-cbind(English.test.1819.VAS.simple$VAS_MCID,probsTest.Mod_XGBTREE.1618.VAS.simple)
names(cal.Mod_XGBTREE.1618.VAS.simple.EngTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.VAS.simple.EngTest<-calibration(VAS_MCID ~ YES, data = cal.Mod_XGBTREE.1618.VAS.simple.EngTest)
cal.plot.Mod_XGBTREE.1618.VAS.simple.EngTest<-plot(cal.obj.Mod_XGBTREE.1618.VAS.simple.EngTest)
cal.ggplot.Mod_XGBTREE.1618.VAS.simple.EngTest<-ggplot(cal.obj.Mod_XGBTREE.1618.VAS.simple.EngTest)
rm(cal.Mod_XGBTREE.1618.VAS.simple.EngTest,cal.obj.Mod_XGBTREE.1618.VAS.simple.EngTest)
## Hosmer-Lemeshow calibration
#head(English.test.1819.VAS.simple$VAS_MCID) ##true outcome
#head(probsTest.Mod_XGBTREE.1618.VAS.simple)[1] ## predicted score
cal.Mod_XGBTREE.1618.VAS.simple.EngTest<-cbind(English.test.1819.VAS.simple$VAS_MCID,
                                               probsTest.Mod_XGBTREE.1618.VAS.simple[1])
names(cal.Mod_XGBTREE.1618.VAS.simple.EngTest)<-c("True_outcome", "Prob_YES")
#head(cal.Mod_XGBTREE.1618.VAS.simple.EngTest)
cal.Mod_XGBTREE.1618.VAS.simple.EngTest$True_outcome_logic <- ifelse(cal.Mod_XGBTREE.1618.VAS.simple.EngTest$True_outcome == "YES", TRUE, FALSE)
#head(cal.Mod_XGBTREE.1618.VAS.simple.EngTest)
#table(cal.Mod_XGBTREE.1618.VAS.simple.EngTest$True_outcome,cal.Mod_XGBTREE.1618.VAS.simple.EngTest$True_outcome_logic)
cal.Mod_XGBTREE.1618.VAS.simple.EngTest$True_outcome_num <- ifelse(cal.Mod_XGBTREE.1618.VAS.simple.EngTest$True_outcome == "YES", 1, 0)
#table(cal.Mod_XGBTREE.1618.VAS.simple.EngTest$True_outcome,
#      cal.Mod_XGBTREE.1618.VAS.simple.EngTest$True_outcome_num)
ResourceSelection::hoslem.test(cal.Mod_XGBTREE.1618.VAS.simple.EngTest$Prob_YES, ## expected/predicted
            cal.Mod_XGBTREE.1618.VAS.simple.EngTest$True_outcome_num) ## observed
#Hosmer and Lemeshow goodness of fit (GOF) test
# X-squared = 3.9657, df = 8, p-value = 0.8602
### Predict classes on test set
pred.Mod_XGBTREE.1618.VAS.simple <- factor(ifelse(probsTest.Mod_XGBTREE.1618.VAS.simple[, "YES"] > 
                                                    best_p_threshold.Mod_XGBTREE.1618.VAS.simple,
                                                  "YES", "NO"))
pred.Mod_XGBTREE.1618.VAS.simple<-relevel(pred.Mod_XGBTREE.1618.VAS.simple, "YES")
#table(pred.Mod_XGBTREE.1618.VAS.simple)
#head(probsTest.Mod_XGBTREE.1618.VAS.simple)
#head(pred.Mod_XGBTREE.1618.VAS.simple) ##YES is predicted when p > 0.5
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_XGBTREE.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_XGBTREE.1618.VAS.simple,
  reference = English.test.1819.VAS.simple$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_XGBTREE.1618.VAS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_XGBTREE.1618.VAS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_XGBTREE.1618.VAS.simple,
  trained.model = Mod_XGBTREE.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_XGBTREE.1618.VAS.simple,
  test_set = English.test.1819.VAS.simple,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_XGBTREE.1618.VAS.simple,
  algorithm ="XGBTREE_VAS_EngTest"
)
#METRICS_Mod_XGBTREE.1618.VAS.simple_EngTest
write.csv(METRICS_Mod_XGBTREE.1618.VAS.simple_EngTest,
          "output/thesis_files/METRICS_Mod_XGBTREE.1618.VAS.hips.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test XGBoost on WQ-VAS hips (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_XGBTREE.1618.VAS.simple <- predict(Mod_XGBTREE.1618.VAS.simple, 
                                                 newdata = AMP_HIPS_CLEANED3.test, type = "prob")

### calibration curves (caret package)
cal.Mod_XGBTREE.1618.VAS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$VAS_MCID,
                                                 probsTest.Mod_XGBTREE.1618.VAS.simple)
names(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.VAS.simple.WelshTest<-calibration(VAS_MCID ~ YES, 
                                                           data = cal.Mod_XGBTREE.1618.VAS.simple.WelshTest)
cal.plot.Mod_XGBTREE.1618.VAS.simple.WelshTest<-plot(cal.obj.Mod_XGBTREE.1618.VAS.simple.WelshTest)
cal.ggplot.Mod_XGBTREE.1618.VAS.simple.WelshTest<-ggplot(cal.obj.Mod_XGBTREE.1618.VAS.simple.WelshTest)
rm(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest, cal.obj.Mod_XGBTREE.1618.VAS.simple.WelshTest)
## Hosmer-Lemeshow calibration
#head(AMP_HIPS_CLEANED3.test$VAS_MCID) ##true outcome
#head(probsTest.Mod_XGBTREE.1618.VAS.simple)[1] ## predicted score
cal.Mod_XGBTREE.1618.VAS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$VAS_MCID,
                                                 probsTest.Mod_XGBTREE.1618.VAS.simple[1])
names(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest)<-c("True_outcome", "Prob_YES")
#head(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest)
cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$True_outcome_logic <- ifelse(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$True_outcome == "YES", TRUE, FALSE)
#head(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest)
table(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$True_outcome,cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$True_outcome_logic)
cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$True_outcome_num <- ifelse(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$True_outcome == "YES", 1, 0)
#table(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$True_outcome,
#      cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$True_outcome_num)
ResourceSelection::hoslem.test(cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$Prob_YES, ## expected/predicted
            cal.Mod_XGBTREE.1618.VAS.simple.WelshTest$True_outcome_num) ## observed
#Hosmer and Lemeshow goodness of fit (GOF) test
# X-squared = 3.5443, df = 8, p-value = 0.8957
### Predict classes on test set
pred.Mod_XGBTREE.1618.VAS.simple <- factor(ifelse(probsTest.Mod_XGBTREE.1618.VAS.simple[, "YES"] > 
                                                    best_p_threshold.Mod_XGBTREE.1618.VAS.simple,
                                                  "YES", "NO")) ##should it be > or >= best_p_threshold?
pred.Mod_XGBTREE.1618.VAS.simple<-relevel(pred.Mod_XGBTREE.1618.VAS.simple, "YES")
#table(pred.Mod_XGBTREE.1618.VAS.simple)
#head(probsTest.Mod_XGBTREE.1618.VAS.simple)
#head(pred.Mod_XGBTREE.1618.VAS.simple) ##NO is predicted when p > 0.45
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_XGBTREE.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_XGBTREE.1618.VAS.simple,
  reference = AMP_HIPS_CLEANED3.test$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_XGBTREE.1618.VAS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_XGBTREE.1618.VAS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_XGBTREE.1618.VAS.simple,
  trained.model = Mod_XGBTREE.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_XGBTREE.1618.VAS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_XGBTREE.1618.VAS.simple,
  algorithm ="XGBTREE_VAS_WelshTest"
)
METRICS_Mod_XGBTREE.1618.VAS.simple_WelshTest
write.csv(METRICS_Mod_XGBTREE.1618.VAS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_XGBTREE.1618.VAS.hips.simple_WelshTest.csv", row.names = FALSE)

# Train Logistic Regression on EQ-VAS hips ----------------------------------------
attach("code/02_ML_main_EQVAS_hips.RData")
English.training.1618.VAS.simple<-English.training.1618.VAS.simple
English.test.1819.VAS.simple<-English.test.1819.VAS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
library(caret)
set.seed(1)
Mod_LR.1618.VAS.simple = train(
  form = VAS_MCID ~ .,
  data = English.training.1618.VAS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           ##sampling = "up", ## outcome class is NOT unbalanced
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "glm",
  family = "binomial",
  metric = 'ROC'
)
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_LR_VAS_simple_EngTest.RData")
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_LR_VAS_simple_WelshTest.RData")
#Mod_LR.1618.VAS.simple
#summary(Mod_LR.1618.VAS.simple)
### Define p threshold
ths.Mod_LR.1618.VAS.simple <- thresholder(Mod_LR.1618.VAS.simple,
                                          threshold = seq(0,1,0.05),
                                          final = TRUE,
                                          statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_LR.1618.VAS.simple<-ths.Mod_LR.1618.VAS.simple[ths.Mod_LR.1618.VAS.simple$J == 
                                                                      max(ths.Mod_LR.1618.VAS.simple$J),
                                                                    "prob_threshold"]
best_p_threshold.Mod_LR.1618.VAS.simple 
### variable importance
plot(varImp(Mod_LR.1618.VAS.simple), top = 10, 
     main="Variable importance (VAS MCID)\nLog Reg (Simple model)") 
#.........................................................................................
#.........................................................................................

# Test Logistic Regression on EQ-VAS hips (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_LR.1618.VAS.simple <- predict(Mod_LR.1618.VAS.simple, 
                                            newdata = English.test.1819.VAS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_LR.1618.VAS.simple.EngTest<-cbind(English.test.1819.VAS.simple$VAS_MCID,
                                          probsTest.Mod_LR.1618.VAS.simple)
names(cal.Mod_LR.1618.VAS.simple.EngTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_LR.1618.VAS.simple.EngTest<-calibration(VAS_MCID ~ YES, 
                                                    data = cal.Mod_LR.1618.VAS.simple.EngTest)
cal.plot.Mod_LR.1618.VAS.simple.EngTest<-plot(cal.obj.Mod_LR.1618.VAS.simple.EngTest)
cal.ggplot.Mod_LR.1618.VAS.simple.EngTest<-ggplot(cal.obj.Mod_LR.1618.VAS.simple.EngTest)
rm(cal.Mod_LR.1618.VAS.simple.EngTest,cal.obj.Mod_LR.1618.VAS.simple.EngTest)
### Predict classes on test set
pred.Mod_LR.1618.VAS.simple <- factor(ifelse(probsTest.Mod_LR.1618.VAS.simple[, "YES"] > 
                                               best_p_threshold.Mod_LR.1618.VAS.simple,
                                             "YES", "NO")) 
pred.Mod_LR.1618.VAS.simple<-relevel(pred.Mod_LR.1618.VAS.simple, "YES")
#table(pred.Mod_LR.1618.VAS.simple)
#head(probsTest.Mod_LR.1618.VAS.simple)
#head(pred.Mod_LR.1618.VAS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_LR.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_LR.1618.VAS.simple,
  reference = English.test.1819.VAS.simple$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_LR.1618.VAS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_LR.1618.VAS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_LR.1618.VAS.simple,
  trained.model = Mod_LR.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_LR.1618.VAS.simple,
  test_set = English.test.1819.VAS.simple,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_LR.1618.VAS.simple,
  algorithm ="LOGREG_VAS_EngTest"
)
#METRICS_Mod_LR.1618.VAS.simple_EngTest
write.csv(METRICS_Mod_LR.1618.VAS.simple_EngTest,
          "output/thesis_files/METRICS_Mod_LR.1618.VAS.hips.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test Logistic Regression on EQ-VAS hips (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_LR.1618.VAS.simple <- predict(Mod_LR.1618.VAS.simple, 
                                            newdata = AMP_HIPS_CLEANED3.test, type = "prob")
### calibration curves (caret package)
cal.Mod_LR.1618.VAS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$VAS_MCID,
                                            probsTest.Mod_LR.1618.VAS.simple)
names(cal.Mod_LR.1618.VAS.simple.WelshTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_LR.1618.VAS.simple.WelshTest<-calibration(VAS_MCID ~ YES, 
                                                      data = cal.Mod_LR.1618.VAS.simple.WelshTest)
cal.plot.Mod_LR.1618.VAS.simple.WelshTest<-plot(cal.obj.Mod_LR.1618.VAS.simple.WelshTest)
cal.ggplot.Mod_LR.1618.VAS.simple.WelshTest<-ggplot(cal.obj.Mod_LR.1618.VAS.simple.WelshTest)
rm(cal.Mod_LR.1618.VAS.simple.WelshTest,cal.obj.Mod_LR.1618.VAS.simple.WelshTest)
### Predict classes on test set
pred.Mod_LR.1618.VAS.simple <- factor(ifelse(probsTest.Mod_LR.1618.VAS.simple[, "YES"] > 
                                               best_p_threshold.Mod_LR.1618.VAS.simple,
                                             "YES", "NO")) 
pred.Mod_LR.1618.VAS.simple<-relevel(pred.Mod_LR.1618.VAS.simple, "YES")
#table(pred.Mod_LR.1618.VAS.simple)
#head(probsTest.Mod_LR.1618.VAS.simple)
#head(pred.Mod_LR.1618.VAS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_LR.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_LR.1618.VAS.simple,
  reference = AMP_HIPS_CLEANED3.test$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_LR.1618.VAS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_LR.1618.VAS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_LR.1618.VAS.simple,
  trained.model = Mod_LR.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_LR.1618.VAS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_LR.1618.VAS.simple,
  algorithm ="LOGREG_VAS_WelshTest"
)
write.csv(METRICS_Mod_LR.1618.VAS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_LR.1618.VAS.hips.simple_WelshTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Train Neural Net on OHS ----------------------------------------
attach("code/02_ML_main_EQVAS_hips.RData")
English.training.1618.VAS.simple<-English.training.1618.VAS.simple
English.test.1819.VAS.simple<-English.test.1819.VAS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
#gc()
#memory.size()
library(caret)
set.seed(1)
Mod_NNET.1618.VAS.simple = train(
  form = VAS_MCID ~ .,
  data = English.training.1618.VAS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           ##sampling = "up", ## outcome class is NOT unbalanced 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "nnet",
  metric = 'ROC' 
)
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_NNET_VAS_simple_EngTest.RData")
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_NNET_VAS_simple_WelshTest.RData")
Mod_NNET.1618.VAS.simple<-Mod_NNET.1618.VAS.simple
#Mod_NNET.1618.VAS.simple
## selected hyperparameters
#head(Mod_NNET.1618.VAS.simple$results[
#  order(Mod_NNET.1618.VAS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_NNET.1618.VAS.simple <- thresholder(Mod_NNET.1618.VAS.simple,
                                            threshold = seq(0,1,0.05),
                                            final = TRUE,
                                            statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_NNET.1618.VAS.simple<-ths.Mod_NNET.1618.VAS.simple[ths.Mod_NNET.1618.VAS.simple$J == 
                                                                          max(ths.Mod_NNET.1618.VAS.simple$J),
                                                                        "prob_threshold"]
best_p_threshold.Mod_NNET.1618.VAS.simple ## p = 0.45
### variable importance
plot(varImp(Mod_NNET.1618.VAS.simple), top = 10, 
     main="Variable importance (VAS MCID)\nNeural Net (Simple model)") 
#.........................................................................................
#.........................................................................................

# Test Neural Net on EQ-VAS hips (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_NNET.1618.VAS.simple <- predict(Mod_NNET.1618.VAS.simple, 
                                              newdata = English.test.1819.VAS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_NNET.1618.VAS.simple.EngTest<-cbind(English.test.1819.VAS.simple$VAS_MCID,
                                            probsTest.Mod_NNET.1618.VAS.simple)
names(cal.Mod_NNET.1618.VAS.simple.EngTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_NNET.1618.VAS.simple.EngTest<-calibration(VAS_MCID ~ YES, 
                                                      data = cal.Mod_NNET.1618.VAS.simple.EngTest)
cal.plot.Mod_NNET.1618.VAS.simple.EngTest<-plot(cal.obj.Mod_NNET.1618.VAS.simple.EngTest)
cal.ggplot.Mod_NNET.1618.VAS.simple.EngTest<-ggplot(cal.obj.Mod_NNET.1618.VAS.simple.EngTest)
rm(cal.Mod_NNET.1618.VAS.simple.EngTest,cal.obj.Mod_NNET.1618.VAS.simple.EngTest)
### Predict classes on test set
pred.Mod_NNET.1618.VAS.simple <- factor(ifelse(probsTest.Mod_NNET.1618.VAS.simple[, "YES"] > 
                                                 best_p_threshold.Mod_NNET.1618.VAS.simple,
                                               "YES", "NO")) 
pred.Mod_NNET.1618.VAS.simple<-relevel(pred.Mod_NNET.1618.VAS.simple, "YES")
#table(pred.Mod_NNET.1618.VAS.simple)
#head(probsTest.Mod_NNET.1618.VAS.simple)
#head(pred.Mod_NNET.1618.VAS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_NNET.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_NNET.1618.VAS.simple,
  reference = English.test.1819.VAS.simple$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_NNET.1618.VAS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_NNET.1618.VAS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_NNET.1618.VAS.simple,
  trained.model = Mod_NNET.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_NNET.1618.VAS.simple,
  test_set = English.test.1819.VAS.simple,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_NNET.1618.VAS.simple,
  algorithm ="NNET_VAS_EngTest"
)
METRICS_Mod_NNET.1618.VAS.simple_EngTest
write.csv(METRICS_Mod_NNET.1618.VAS.simple_EngTest,
          "MODELS_METRICS/OHS-VAS/simple/METRICS_Mod_NNET.1618.VAS.hips.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test Neural Net on EQ-VAS hips (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_NNET.1618.VAS.simple <- predict(Mod_NNET.1618.VAS.simple, 
                                              newdata = AMP_HIPS_CLEANED3.test, type = "prob")
### calibration curves (caret package)
cal.Mod_NNET.1618.VAS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$VAS_MCID,
                                              probsTest.Mod_NNET.1618.VAS.simple)
names(cal.Mod_NNET.1618.VAS.simple.WelshTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_NNET.1618.VAS.simple.WelshTest<-calibration(VAS_MCID ~ YES, 
                                                        data = cal.Mod_NNET.1618.VAS.simple.WelshTest)
cal.plot.Mod_NNET.1618.VAS.simple.WelshTest<-plot(cal.obj.Mod_NNET.1618.VAS.simple.WelshTest)
cal.ggplot.Mod_NNET.1618.VAS.simple.WelshTest<-ggplot(cal.obj.Mod_NNET.1618.VAS.simple.WelshTest)
rm(cal.Mod_NNET.1618.VAS.simple.WelshTest,cal.obj.Mod_NNET.1618.VAS.simple.WelshTest)
### Predict classes on test set
pred.Mod_NNET.1618.VAS.simple <- factor(ifelse(probsTest.Mod_NNET.1618.VAS.simple[, "YES"] > 
                                                 best_p_threshold.Mod_NNET.1618.VAS.simple,
                                               "YES", "NO")) 
pred.Mod_NNET.1618.VAS.simple<-relevel(pred.Mod_NNET.1618.VAS.simple, "YES")
#table(pred.Mod_NNET.1618.VAS.simple)
#head(probsTest.Mod_NNET.1618.VAS.simple)
#head(pred.Mod_NNET.1618.VAS.simple) ##NO is predicted when p > 0.45
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_NNET.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_NNET.1618.VAS.simple,
  reference = AMP_HIPS_CLEANED3.test$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_NNET.1618.VAS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_NNET.1618.VAS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_NNET.1618.VAS.simple,
  trained.model = Mod_NNET.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_NNET.1618.VAS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_NNET.1618.VAS.simple,
  algorithm ="NNET_VAS_WelshTest"
)
write.csv(METRICS_Mod_NNET.1618.VAS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_NNET.1618.VAS.hips.simple_WelshTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Train Random Forest on OHS ----------------------------------------
attach("code/02_ML_main_EQVAS_hips.RData")
English.training.1618.VAS.simple<-English.training.1618.VAS.simple
English.test.1819.VAS.simple<-English.test.1819.VAS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
#gc()
#memory.size()
library(caret)
set.seed(1)
Mod_RF.1618.VAS.simple = train(
  form = VAS_MCID ~ .,
  data = English.training.1618.VAS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           ## sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "rf",
  metric = 'ROC' 
)
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_RF_VAS_simple_EngTest.RData")
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_RF_VAS_simple_WelshTest.RData")
#Mod_RF.1618.VAS.simple
## selected hyperparameters
#head(Mod_RF.1618.VAS.simple$results[
#  order(Mod_RF.1618.VAS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_RF.1618.VAS.simple <- thresholder(Mod_RF.1618.VAS.simple,
                                          threshold = seq(0,1,0.05),
                                          final = TRUE,
                                          statistics = "all")
values_ths<-c("prob_threshold", "Sensitivity", "Specificity", "J","Balanced Accuracy", 
              "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1", 
              "Prevalence", "Detection Rate", "Detection Prevalence", "Accuracy", 
              "Kappa", "Dist")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_RF.1618.VAS.simple<-ths.Mod_RF.1618.VAS.simple[ths.Mod_RF.1618.VAS.simple$J == 
                                                                      max(ths.Mod_RF.1618.VAS.simple$J),
                                                                    "prob_threshold"]
best_p_threshold.Mod_RF.1618.VAS.simple ## p = 0.45
### variable importance
plot(varImp(Mod_RF.1618.VAS.simple), top = 10, 
     main="Variable importance (VAS MCID)\nRandom Forest (Simple model)") 
#.........................................................................................
#.........................................................................................

# Test Random Forest on EQ-VAS hips (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_RF.1618.VAS.simple <- predict(Mod_RF.1618.VAS.simple, 
                                            newdata = English.test.1819.VAS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_RF.1618.VAS.simple.EngTest<-cbind(English.test.1819.VAS.simple$VAS_MCID,
                                          probsTest.Mod_RF.1618.VAS.simple)
names(cal.Mod_RF.1618.VAS.simple.EngTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_RF.1618.VAS.simple.EngTest<-calibration(VAS_MCID ~ YES, 
                                                    data = cal.Mod_RF.1618.VAS.simple.EngTest)
cal.plot.Mod_RF.1618.VAS.simple.EngTest<-plot(cal.obj.Mod_RF.1618.VAS.simple.EngTest)
cal.ggplot.Mod_RF.1618.VAS.simple.EngTest<-ggplot(cal.obj.Mod_RF.1618.VAS.simple.EngTest)
rm(cal.Mod_RF.1618.VAS.simple.EngTest,cal.obj.Mod_RF.1618.VAS.simple.EngTest)
### Predict classes on test set
pred.Mod_RF.1618.VAS.simple <- factor(ifelse(probsTest.Mod_RF.1618.VAS.simple[, "YES"] > 
                                               best_p_threshold.Mod_RF.1618.VAS.simple,
                                             "YES", "NO")) 
pred.Mod_RF.1618.VAS.simple<-relevel(pred.Mod_RF.1618.VAS.simple, "YES")
#table(pred.Mod_RF.1618.VAS.simple)
#head(probsTest.Mod_RF.1618.VAS.simple)
#head(pred.Mod_RF.1618.VAS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_RF.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_RF.1618.VAS.simple,
  reference = English.test.1819.VAS.simple$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_RF.1618.VAS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_RF.1618.VAS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_RF.1618.VAS.simple,
  trained.model = Mod_RF.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_RF.1618.VAS.simple,
  test_set = English.test.1819.VAS.simple,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_RF.1618.VAS.simple,
  algorithm ="RF_VAS_EngTest"
)
#METRICS_Mod_RF.1618.VAS.simple_EngTest
write.csv(METRICS_Mod_RF.1618.VAS.simple_EngTest,
          "output/thesis_files/METRICS_Mod_RF.1618.VAS.hips.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test Random Forest on EQ-VAS hips (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_RF.1618.VAS.simple <- predict(Mod_RF.1618.VAS.simple, 
                                            newdata = AMP_HIPS_CLEANED3.test, type = "prob")
### calibration curves (caret package)
cal.Mod_RF.1618.VAS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$VAS_MCID,
                                            probsTest.Mod_RF.1618.VAS.simple)
names(cal.Mod_RF.1618.VAS.simple.WelshTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_RF.1618.VAS.simple.WelshTest<-calibration(VAS_MCID ~ YES, 
                                                      data = cal.Mod_RF.1618.VAS.simple.WelshTest)
cal.plot.Mod_RF.1618.VAS.simple.WelshTest<-plot(cal.obj.Mod_RF.1618.VAS.simple.WelshTest)
cal.ggplot.Mod_RF.1618.VAS.simple.WelshTest<-ggplot(cal.obj.Mod_RF.1618.VAS.simple.WelshTest)
rm(cal.Mod_RF.1618.VAS.simple.WelshTest,cal.obj.Mod_RF.1618.VAS.simple.WelshTest)
### Predict classes on test set
pred.Mod_RF.1618.VAS.simple <- factor(ifelse(probsTest.Mod_RF.1618.VAS.simple[, "YES"] > 
                                               best_p_threshold.Mod_RF.1618.VAS.simple,
                                             "YES", "NO")) 
pred.Mod_RF.1618.VAS.simple<-relevel(pred.Mod_RF.1618.VAS.simple, "YES")
#table(pred.Mod_RF.1618.VAS.simple)
#head(probsTest.Mod_RF.1618.VAS.simple)
#head(pred.Mod_RF.1618.VAS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_RF.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_RF.1618.VAS.simple,
  reference = AMP_HIPS_CLEANED3.test$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_RF.1618.VAS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_RF.1618.VAS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_RF.1618.VAS.simple,
  trained.model = Mod_RF.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_RF.1618.VAS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_RF.1618.VAS.simple,
  algorithm ="RF_VAS_WelshTest"
)
write.csv(METRICS_Mod_RF.1618.VAS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_RF.1618.VAS.hips.simple_WelshTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Train Elastic Net on EQ-VAS hips ----------------------------------------
attach("code/02_ML_main_EQVAS_hips.RData")
English.training.1618.VAS.simple<-English.training.1618.VAS.simple
English.test.1819.VAS.simple<-English.test.1819.VAS.simple
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
#gc()
#memory.size()
set.seed(1)
Mod_GLMNET.1618.VAS.simple = train(
  form = VAS_MCID ~ .,
  data = English.training.1618.VAS.simple,
  trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                           ##sampling = "up", ## outcome class is unbalanced (possibly specify better this), so up-sampling is required!
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary, ## this is required for selecting by AUROC metric https://cran.r-project.org/web/packages/caret/vignettes/caret.html
                           savePredictions = T,
                           verboseIter = TRUE), 
  method = "glmnet",
  metric = 'ROC' 
)
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_GLMNET_VAS_simple_EngTest.RData")
#load("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_GLMNET_VAS_simple_WelshTest.RData")
Mod_GLMNET.1618.VAS.simple
#summary(Mod_GLMNET.1618.VAS.simple)
## selected hyperparameters
#head(Mod_GLMNET.1618.VAS.simple$results[
#  order(Mod_GLMNET.1618.VAS.simple$results$ROC, decreasing = TRUE),],1)
### Define p threshold
ths.Mod_GLMNET.1618.VAS.simple <- thresholder(Mod_GLMNET.1618.VAS.simple,
                                              threshold = seq(0,1,0.05),
                                              final = TRUE,
                                              statistics = "all")
#extracting the p threshold associated to the highest J metric:
best_p_threshold.Mod_GLMNET.1618.VAS.simple<-ths.Mod_GLMNET.1618.VAS.simple[ths.Mod_GLMNET.1618.VAS.simple$J == 
                                                                              max(ths.Mod_GLMNET.1618.VAS.simple$J),
                                                                            "prob_threshold"]
best_p_threshold.Mod_GLMNET.1618.VAS.simple ## p = 0.4
### variable importance
plot(varImp(Mod_GLMNET.1618.VAS.simple), top = 10, 
     main="Variable importance (VAS MCID)\nElastic Net (Simple model)") 
#.........................................................................................
#.........................................................................................

# Test Elastic Net on EQ-VAS hips (English Test set)----------------------------------------
### Predicting probability scores on English test set
probsTest.Mod_GLMNET.1618.VAS.simple <- predict(Mod_GLMNET.1618.VAS.simple, 
                                                newdata = English.test.1819.VAS.simple, type = "prob")
### calibration curves (caret package)
cal.Mod_GLMNET.1618.VAS.simple.EngTest<-cbind(English.test.1819.VAS.simple$VAS_MCID,
                                              probsTest.Mod_GLMNET.1618.VAS.simple)
names(cal.Mod_GLMNET.1618.VAS.simple.EngTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_GLMNET.1618.VAS.simple.EngTest<-calibration(VAS_MCID ~ YES, 
                                                        data = cal.Mod_GLMNET.1618.VAS.simple.EngTest)
cal.plot.Mod_GLMNET.1618.VAS.simple.EngTest<-plot(cal.obj.Mod_GLMNET.1618.VAS.simple.EngTest)
cal.ggplot.Mod_GLMNET.1618.VAS.simple.EngTest<-ggplot(cal.obj.Mod_GLMNET.1618.VAS.simple.EngTest)
rm(cal.Mod_GLMNET.1618.VAS.simple.EngTest,cal.obj.Mod_GLMNET.1618.VAS.simple.EngTest)
### Predict classes on test set
pred.Mod_GLMNET.1618.VAS.simple <- factor(ifelse(probsTest.Mod_GLMNET.1618.VAS.simple[, "YES"] > 
                                                   best_p_threshold.Mod_GLMNET.1618.VAS.simple,
                                                 "YES", "NO")) 
pred.Mod_GLMNET.1618.VAS.simple<-relevel(pred.Mod_GLMNET.1618.VAS.simple, "YES")
#table(pred.Mod_GLMNET.1618.VAS.simple)
#head(probsTest.Mod_GLMNET.1618.VAS.simple)
#head(pred.Mod_GLMNET.1618.VAS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_GLMNET.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_GLMNET.1618.VAS.simple,
  reference = English.test.1819.VAS.simple$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_GLMNET.1618.VAS.simple$table
### Extract relevant training and testing (English test set) metrics using custom-made function
METRICS_Mod_GLMNET.1618.VAS.simple_EngTest<-ExtractMetrics(
  thresholder.output = ths.Mod_GLMNET.1618.VAS.simple,
  trained.model = Mod_GLMNET.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_GLMNET.1618.VAS.simple,
  test_set = English.test.1819.VAS.simple,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_GLMNET.1618.VAS.simple,
  algorithm ="GLMNET_VAS_EngTest"
)
#METRICS_Mod_GLMNET.1618.VAS.simple_EngTest
write.csv(METRICS_Mod_GLMNET.1618.VAS.simple_EngTest,
          "output/thesis_files/METRICS_Mod_GLMNET.1618.VAS.hips.simple_EngTest.csv", row.names = FALSE)
#.........................................................................................
#.........................................................................................

# Test Random Forest on EQ-VAS hips (Welsh Test set)----------------------------------------
### Predicting probability scores on Welsh test set
probsTest.Mod_GLMNET.1618.VAS.simple <- predict(Mod_GLMNET.1618.VAS.simple, 
                                                newdata = AMP_HIPS_CLEANED3.test, type = "prob")
### calibration curves (caret package)
cal.Mod_GLMNET.1618.VAS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$VAS_MCID,
                                                probsTest.Mod_GLMNET.1618.VAS.simple)
names(cal.Mod_GLMNET.1618.VAS.simple.WelshTest)<-c("VAS_MCID","YES","NO")
cal.obj.Mod_GLMNET.1618.VAS.simple.WelshTest<-calibration(VAS_MCID ~ YES, 
                                                          data = cal.Mod_GLMNET.1618.VAS.simple.WelshTest)
cal.plot.Mod_GLMNET.1618.VAS.simple.WelshTest<-plot(cal.obj.Mod_GLMNET.1618.VAS.simple.WelshTest)
cal.ggplot.Mod_GLMNET.1618.VAS.simple.WelshTest<-ggplot(cal.obj.Mod_GLMNET.1618.VAS.simple.WelshTest)
rm(cal.Mod_GLMNET.1618.VAS.simple.WelshTest,cal.obj.Mod_GLMNET.1618.VAS.simple.WelshTest)
### Predict classes on test set
pred.Mod_GLMNET.1618.VAS.simple <- factor(ifelse(probsTest.Mod_GLMNET.1618.VAS.simple[, "YES"] > 
                                                   best_p_threshold.Mod_GLMNET.1618.VAS.simple,
                                                 "YES", "NO")) 
pred.Mod_GLMNET.1618.VAS.simple<-relevel(pred.Mod_GLMNET.1618.VAS.simple, "YES")
#table(pred.Mod_GLMNET.1618.VAS.simple)
#head(probsTest.Mod_GLMNET.1618.VAS.simple)
#head(pred.Mod_GLMNET.1618.VAS.simple) 
### Create confusion matrix for predictions on test set
conf.Matrix.Mod_GLMNET.1618.VAS.simple<-confusionMatrix(
  data = pred.Mod_GLMNET.1618.VAS.simple,
  reference = AMP_HIPS_CLEANED3.test$VAS_MCID,
  positive = "YES")
#conf.Matrix.Mod_GLMNET.1618.VAS.simple$table
### Extract relevant training and testing (Welsh test set) metrics using custom-made function
METRICS_Mod_GLMNET.1618.VAS.simple_WelshTest<-ExtractMetrics(
  thresholder.output = ths.Mod_GLMNET.1618.VAS.simple,
  trained.model = Mod_GLMNET.1618.VAS.simple,
  test_set.model_probabilities = probsTest.Mod_GLMNET.1618.VAS.simple,
  test_set = AMP_HIPS_CLEANED3.test,
  OutputVar = "VAS_MCID",
  test_set.confusion_matrix = conf.Matrix.Mod_GLMNET.1618.VAS.simple,
  algorithm ="GLMNET_VAS_WelshTest"
)
write.csv(METRICS_Mod_GLMNET.1618.VAS.simple_WelshTest,
          "output/thesis_files/METRICS_Mod_GLMNET.1618.VAS.hips.simple_WelshTest.csv", row.names = FALSE)






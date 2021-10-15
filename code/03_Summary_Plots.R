# Main model: Variable Importance -----------------------------------------------------
## OHS MCID Simple model
attach("code/models/OHS_simple/HIPS_ML_training_1618_XGBTREE_simple_EngTest.RData")
Mod_XGBTREE.1618.OHS.simple<-Mod_XGBTREE.1618.OHS.simple
VarImp.Mod_XGBTREE.OHS.simple<-plot(caret::varImp(Mod_XGBTREE.1618.OHS.simple), top = 10,
                                    xlab="Variable Importance",
                                    main="Prediction of OHS MCID - Hip dataset") 
rm(Mod_XGBTREE.1618.OHS.simple)
## OHS-VAS MCID Simple model
attach("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_XGBTREE_VAS_simple_EngTest.RData")
Mod_XGBTREE.1618.VAS.simple.hips<-Mod_XGBTREE.1618.VAS.simple
VarImp.Mod_XGBTREE.VAS.simple.hips<-plot(caret::varImp(Mod_XGBTREE.1618.VAS.simple.hips), top = 10,
                                         xlab="Variable Importance",
                                         main="Prediction of EQ-VAS MCID - Hip Dataset") 
rm(Mod_XGBTREE.1618.VAS.simple.hips)
## OKS MCID Simple model
attach("code/models/OKS_simple/KNEES_ML_training_1618_XGBTREE_simple_EngTest.RData")
Mod_XGBTREE.OKS.simple<-Mod_XGBTREE.OKS.simple
VarImp.Mod_XGBTREE.OKS.simple<-plot(caret::varImp(Mod_XGBTREE.OKS.simple), top = 10, 
                                    xlab="Variable Importance",
                                    main="Prediction of OKS MCID - Knee dataset") 
rm(Mod_XGBTREE.OKS.simple)
## OKS-VAS MCID Simple model
attach("code/models/EQVAS_knees_simple/KNEES_ML_training_1618_XGBTREE_VAS_simple_EngTest.RData")
Mod_XGBTREE.1618.VAS.simple.knee<-Mod_XGBTREE.1618.VAS.simple
VarImp.Mod_XGBTREE.VAS.simple.knees<-plot(caret::varImp(Mod_XGBTREE.1618.VAS.simple.knee), top = 10, 
                                          xlab="Variable Importance", 
                                          main="Prediction of EQ-VAS MCID - Knee dataset") 
rm(Mod_XGBTREE.1618.VAS.simple.knee)
## plot
ggpubr::ggarrange(VarImp.Mod_XGBTREE.OHS.simple,
          VarImp.Mod_XGBTREE.VAS.simple.hips,
          VarImp.Mod_XGBTREE.OKS.simple,
          VarImp.Mod_XGBTREE.VAS.simple.knees,
          ncol = 2, 
          nrow = 2)
## width = 1000, height = 700   
#.........................................................................................
#.........................................................................................

# Main model: ROC curves -----------------------------------------------------
### ROC curves: input data
##### compare AUROC curves for best models (XGBTREE)
## OHS MCID: ROC object for main model (18 predictors) - English test
attach("code/models/OHS_simple/HIPS_ML_training_1618_XGBTREE_simple_EngTest.RData")
English.test.1819.OHS.simple<-English.test.1819.OHS.simple
probsTest.Mod_XGBTREE.1618.OHS.simple.Eng<-probsTest.Mod_XGBTREE.1618.OHS.simple
ROCR.Mod.1618.XGBTREE.simple.Eng <- ROCR::prediction(
  predictions = probsTest.Mod_XGBTREE.1618.OHS.simple.Eng$YES,
  labels = English.test.1819.OHS.simple$OHS_MCID)
rm(English.test.1819.OHS.simple,probsTest.Mod_XGBTREE.1618.OHS.simple.Eng)
## OHS MCID: ROC object for main model (18 predictors) - Welsh test
attach("code/models/OHS_simple/HIPS_ML_training_1618_XGBTREE_simple_WelshTest.RData")
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
probsTest.Mod_XGBTREE.1618.OHS.simple.Welsh<-probsTest.Mod_XGBTREE.1618.OHS.simple
ROCR.Mod.1618.XGBTREE.simple.Welsh <- ROCR::prediction(
  predictions = probsTest.Mod_XGBTREE.1618.OHS.simple.Welsh$YES,
  labels = AMP_HIPS_CLEANED3.test$OHS_MCID)
rm(AMP_HIPS_CLEANED3.test,probsTest.Mod_XGBTREE.1618.OHS.simple.Welsh)
## HIPS EQVAS MCID: ROC object for simple model - English test
attach("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_XGBTREE_VAS_simple_EngTest.RData")
English.test.1819.VAS.hips.simple<-English.test.1819.VAS.simple
probsTest.Mod_XGBTREE.1618.VAS.hips.simple.Eng<-probsTest.Mod_XGBTREE.1618.VAS.simple
ROCR.Mod.1618.XGBTREE.VAS.hips.simple.Eng <- ROCR::prediction(
  predictions = probsTest.Mod_XGBTREE.1618.VAS.hips.simple.Eng$YES,
  labels = English.test.1819.VAS.hips.simple$VAS_MCID)
rm(English.test.1819.VAS.hips.simple,probsTest.Mod_XGBTREE.1618.VAS.hips.simple.Eng)
## HIPS EQVAS MCID: ROC object for simple model - Welsh test
attach("code/models/EQVAS_hips_simple/HIPS_ML_training_1618_XGBTREE_VAS_simple_WelshTest.RData")
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
probsTest.Mod_XGBTREE.1618.VAS.hips.simple.Welsh<-probsTest.Mod_XGBTREE.1618.VAS.simple
ROCR.Mod.1618.XGBTREE.VAS.hips.simple.Welsh <- ROCR::prediction(
  predictions = probsTest.Mod_XGBTREE.1618.VAS.hips.simple.Welsh$YES,
  labels = AMP_HIPS_CLEANED3.test$VAS_MCID)
rm(AMP_HIPS_CLEANED3.test,probsTest.Mod_XGBTREE.1618.VAS.hips.simple.Welsh)
## OKS MCID: ROC object for simple model - English test
attach("code/models/OKS_simple/KNEES_ML_training_1618_XGBTREE_simple_EngTest.RData")
English.test.1819.OKS.simple<-English.test.1819.OKS.simple
probsTest.Mod_XGBTREE.1618.OKS.simple.Eng<-probsTest.Mod_XGBTREE.1618.OKS.simple
ROCR.Mod_XGBTREE.1618.OKS.simple.Eng <- ROCR::prediction(
  predictions = probsTest.Mod_XGBTREE.1618.OKS.simple.Eng$YES,
  labels = English.test.1819.OKS.simple$OKS_MCID)
rm(English.test.1819.OKS.simple,probsTest.Mod_XGBTREE.1618.OKS.simple.Eng)
## OKS MCID: ROC object for simple model - Welsh test
attach("code/models/OKS_simple/KNEES_ML_training_1618_XGBTREE_simple_WelshTest.RData")
AMP_KNEES_CLEANED3.test<-AMP_KNEES_CLEANED3.test
probsTest.Mod_XGBTREE.1618.OKS.simple.Welsh<-probsTest.Mod_XGBTREE.1618.OKS.simple
ROCR.Mod_XGBTREE.1618.OKS.simple.Welsh <- ROCR::prediction(
  predictions = probsTest.Mod_XGBTREE.1618.OKS.simple.Welsh$YES,
  labels = AMP_KNEES_CLEANED3.test$OKS_MCID)
rm(AMP_KNEES_CLEANED3.test,probsTest.Mod_XGBTREE.1618.OKS.simple.Welsh)
## KNEES EQVAS MCID: ROC object for simple model - English test
attach("code/models/EQVAS_knees_simple/KNEES_ML_training_1618_XGBTREE_VAS_simple_EngTest.RData")
English.test.1819.VAS.knees.simple.Eng<-English.test.1819
probsTest.Mod_XGBTREE.1618.VAS.knees.simple.Eng<-probsTest.Mod_XGBTREE.1618.VAS.simple
ROCR.Mod.1618.XGBTREE.VAS.knees.simple.Eng <- ROCR::prediction(
  predictions = probsTest.Mod_XGBTREE.1618.VAS.knees.simple.Eng$YES,
  labels = English.test.1819.VAS.knees.simple.Eng$VAS_MCID)
rm(English.test.1819.VAS.knees.simple.Eng,probsTest.Mod_XGBTREE.1618.VAS.knees.simple.Eng)
## ROC object for simple model - Welsh test
attach("code/models/EQVAS_knees_simple/KNEES_ML_training_1618_XGBTREE_VAS_simple_WelshTest.RData")
AMP_KNEES_CLEANED3.test<-AMP_KNEES_CLEANED3.test
probsTest.Mod_XGBTREE.1618.VAS.knees.simple.Welsh<-probsTest.Mod_XGBTREE.1618.VAS.simple
ROCR.Mod_LR.1618.XGBTREE.VAS.knees.simple.Welsh <- ROCR::prediction(
  predictions = probsTest.Mod_XGBTREE.1618.VAS.knees.simple.Welsh$YES,
  labels = AMP_KNEES_CLEANED3.test$VAS_MCID)
rm(AMP_KNEES_CLEANED3.test,probsTest.Mod_XGBTREE.1618.VAS.knees.simple.Welsh)
#### ROC curves: English vs Welsh test sets (main model with 18 predictors)
par(mfrow=c(2,2))
## OHS MCID ROCs (Hips)
plot(
  ROCR::performance(ROCR.Mod.1618.XGBTREE.simple.Eng, measure = "sens", x.measure = "fpr"),
  col = "#ED0000FF", lwd = 2)
plot(
  ROCR::performance(ROCR.Mod.1618.XGBTREE.simple.Welsh, measure = "sens", x.measure = "fpr"),
  add = TRUE,
  col = "#42B540FF", lwd = 2
)
segments(0,0,1,1,col="black", lty=3) ## add the diagonal
title("Prediction of OHS MCID\n(Hip dataset)")
## EQ-VAS MCID ROCs (Hips)
plot(
  ROCR::performance(ROCR.Mod.1618.XGBTREE.VAS.hips.simple.Eng, measure = "sens", x.measure = "fpr"),
  col = "#ED0000FF", lwd = 2)
plot(
  ROCR::performance(ROCR.Mod.1618.XGBTREE.VAS.hips.simple.Welsh, measure = "sens", x.measure = "fpr"),
  add = TRUE,
  col = "#42B540FF", lwd = 2
)
segments(0,0,1,1,col="black", lty=3) ## add the diagonal
title("Prediction of EQ-VAS MCID\n(Hip dataset)")
## OKS MCID ROCs (Knees)
plot(
  ROCR::performance(ROCR.Mod_XGBTREE.1618.OKS.simple.Eng, measure = "sens", x.measure = "fpr"),
  col = "#ED0000FF", lwd = 2)
plot(
  ROCR::performance(ROCR.Mod_XGBTREE.1618.OKS.simple.Welsh, measure = "sens", x.measure = "fpr"),
  add = TRUE,
  col = "#42B540FF", lwd = 2
)
segments(0,0,1,1,col="black", lty=3) ## add the diagonal
title("Prediction of OKS MCID\n(Knee dataset)")
## EQ-VAS MCID ROCs (Knees)
plot(
  ROCR::performance(ROCR.Mod.1618.XGBTREE.VAS.knees.simple.Eng, measure = "sens", x.measure = "fpr"),
  col = "#ED0000FF", lwd = 2)
plot(
  ROCR::performance(ROCR.Mod_LR.1618.XGBTREE.VAS.knees.simple.Welsh, measure = "sens", x.measure = "fpr"),
  add = TRUE,
  col = "#42B540FF", lwd = 2
)
segments(0,0,1,1,col="black", lty=3) ## add the diagonal
title("Prediction of EQ-VAS MCID\n(Knee dataset)")
par(mar=c(0.1, 0.1, 0.1, 0.1))
legend(x=0.5,y=0.7,
       legend = c("English Test set",
                  "Welsh Test set"),
       col = c("#ED0000FF", "#42B540FF"),
       lty = c(1, 1),
       bty = "n",
       cex=0.7,
       xpd = TRUE
)
## width = 800, height = 800 (ROC_curves_main_models.png)  
#.........................................................................................
#.........................................................................................

# Secondary model: ROC curves -----------------------------------------------------
#### ROC curves: English 18 vs 39 predictors *****to restructure

par(mfrow=c(2,2))

## OHS MCID ROCs (Hips)
plot(
  performance(ROCR.Mod_LR.1618.XGBTREE.simple.Eng_OHS, measure = "sens", x.measure = "fpr"),
  col = "#ED0000FF", lwd = 2)


plot(
  performance(ROCR.Mod_LR.1618.XGBTREE.complex_OHS, measure = "sens", x.measure = "fpr"),
  add = TRUE,
  col = "#00468BFF", lwd = 2
)

segments(0,0,1,1,col="black", lty=3) ## add the diagonal
title("Prediction of OHS MCID\n(Hip dataset)")

## EQ-VAS MCID ROCs (Hips)
plot(
  performance(ROCR.Mod_LR.1618.XGBTREE.simple.Eng_VAS, measure = "sens", x.measure = "fpr"),
  col = "#ED0000FF", lwd = 2)

plot(
  performance(ROCR.Mod_LR.1618.XGBTREE.complex_VAS, measure = "sens", x.measure = "fpr"),
  add = TRUE,
  col = "#00468BFF", lwd = 2
)
segments(0,0,1,1,col="black", lty=3) ## add the diagonal
title("Prediction of EQ-VAS MCID\n(Hip dataset)")

## OKS MCID ROCs (Knees)

plot(
  performance(ROCR.Mod_XGBTREE.1618.OKS.simple.Eng, measure = "sens", x.measure = "fpr"),
  col = "#ED0000FF", lwd = 2)

plot(
  performance(ROCR.Mod_XGBTREE.1618.OKS.complex, measure = "sens", x.measure = "fpr"),
  add = TRUE,
  col = "#00468BFF", lwd = 2
)

segments(0,0,1,1,col="black", lty=3) ## add the diagonal
title("Prediction of OKS MCID\n(Knee dataset)")


plot(
  performance(ROCR.Mod_XGBTREE.1618.OKSVAS.simple.Eng, measure = "sens", x.measure = "fpr"),
  col = "#ED0000FF", lwd = 2)


plot(
  performance(ROCR.Mod_XGBTREE.1618.OKSVAS.complex, measure = "sens", x.measure = "fpr"),
  add = TRUE,
  col = "#00468BFF", lwd = 2
)

segments(0,0,1,1,col="black", lty=3) ## add the diagonal
title("Prediction of EQ-VAS MCID\n(Knee dataset)")
par(mar=c(0.1, 0.1, 0.1, 0.1))
legend(x=0.5,y=0.7,
       legend = c("English Test set (39 predictors)", "English Test set (18 predictors)"),
       col = c("#00468BFF", "#ED0000FF"),
       lty = c(1, 1),
       bty = "n",
       cex=0.7,
       xpd = TRUE
)

## width = 800, height = 800   
#.........................................................................................
#.........................................................................................

# Main model: Calibration curves -----------------------------------------------------
## calibration object for simple model - English test
library(caret)
attach("code/models/OHS_simple/HIPS_ML_training_1618_XGBTREE_simple_EngTest.RData")
#cal.plot.Mod_XGBTREE.1618.OHS.simple.EngTest<-cal.plot.Mod_XGBTREE.1618.OHS.simple.EngTest
English.test.1819.OHS.simple<-English.test.1819.OHS.simple
probsTest.Mod_XGBTREE.1618.OHS.simple.Eng<-probsTest.Mod_XGBTREE.1618.OHS.simple
cal.Mod_XGBTREE.1618.OHS.simple.EngTest<-cbind(English.test.1819.OHS.simple$OHS_MCID,
                                               probsTest.Mod_XGBTREE.1618.OHS.simple.Eng)
names(cal.Mod_XGBTREE.1618.OHS.simple.EngTest)<-c("OHS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest<-caret::calibration(OHS_MCID ~ YES, data = cal.Mod_XGBTREE.1618.OHS.simple.EngTest)
plot(cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest)

## calibration object for simple model - Welsh test
attach("code/models/OHS_simple/HIPS_ML_training_1618_XGBTREE_simple_WelshTest.RData")
library(caret)
AMP_HIPS_CLEANED3.test<-AMP_HIPS_CLEANED3.test
probsTest.Mod_XGBTREE.1618.OHS.simple.Welsh<-probsTest.Mod_XGBTREE.1618.OHS.simple
cal.Mod_XGBTREE.1618.OHS.simple.WelshTest<-cbind(AMP_HIPS_CLEANED3.test$OHS_MCID,
                                                 probsTest.Mod_XGBTREE.1618.OHS.simple.Welsh)
names(cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)<-c("OHS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest<-calibration(OHS_MCID ~ YES, 
                                                           data = cal.Mod_XGBTREE.1618.OHS.simple.WelshTest)
plot(cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest)

## calibration object for complex model
attach("H:/MSc_thesis_models/NEW/complex_hips_models_5fold_3rep/HIPS_ML_training_1618_XGBTREE_complex_5fold_3rep.RData")
library(caret)
cal.Mod_XGBTREE.1618.OHS.complex.EngTest<-cbind(English.test.1819$OHS_MCID,
                                                probsTest.Mod_XGBTREE.1618.OHS.complex)
names(cal.Mod_XGBTREE.1618.OHS.complex.EngTest)<-c("OHS_MCID","YES","NO")
cal.obj.Mod_XGBTREE.1618.OHS.complex.EngTest<-calibration(OHS_MCID ~ YES, data = cal.Mod_XGBTREE.1618.OHS.complex.EngTest)
plot(cal.obj.Mod_XGBTREE.1618.OHS.complex.EngTest)

### the x points are always the same...
cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest$data$midpoint
# [1]  4.545455 13.636364 22.727273 31.818182 40.909091 50.000000 59.090909 68.181818 77.272727 86.363636
# [11] 95.454545
cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest$data$midpoint
# [1]  4.545455 13.636364 22.727273 31.818182 40.909091 50.000000 59.090909 68.181818 77.272727 86.363636
# [11] 95.454545
cal.obj.Mod_XGBTREE.1618.OHS.complex.EngTest$data$midpoint
# [1]  4.545455 13.636364 22.727273 31.818182 40.909091 50.000000 59.090909 68.181818 77.272727 86.363636
# [11] 95.454545

###*** create a function for this

## create dataframe to plot overlapping lines (I cannot use directly the function plot for this...)
calibration.df.OHS.simple.Eng<-as.data.frame(cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest$data$midpoint)
names(calibration.df.OHS.simple.Eng)<-c("Pred.Score")
calibration.df.OHS.simple.Eng$obs.perc<-cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest$data$Percent
calibration.df.OHS.simple.Eng$obs.perc.lower<-cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest$data$Lower
calibration.df.OHS.simple.Eng$obs.perc.upper<-cal.obj.Mod_XGBTREE.1618.OHS.simple.EngTest$data$Upper
calibration.df.OHS.simple.Eng$Model<-"English Test (18 predictors)"

calibration.df.OHS.simple.Welsh<-as.data.frame(cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest$data$midpoint)
names(calibration.df.OHS.simple.Welsh)<-c("Pred.Score")
calibration.df.OHS.simple.Welsh$obs.perc<-cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest$data$Percent
calibration.df.OHS.simple.Welsh$obs.perc.lower<-cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest$data$Lower
calibration.df.OHS.simple.Welsh$obs.perc.upper<-cal.obj.Mod_XGBTREE.1618.OHS.simple.WelshTest$data$Upper
calibration.df.OHS.simple.Welsh$Model<-"Welsh Test (18 predictors)"

calibration.df.OHS.complex.Eng<-as.data.frame(cal.obj.Mod_XGBTREE.1618.OHS.complex.EngTest$data$midpoint)
names(calibration.df.OHS.complex.Eng)<-c("Pred.Score")
calibration.df.OHS.complex.Eng$obs.perc<-cal.obj.Mod_XGBTREE.1618.OHS.complex.EngTest$data$Percent
calibration.df.OHS.complex.Eng$obs.perc.lower<-cal.obj.Mod_XGBTREE.1618.OHS.complex.EngTest$data$Lower
calibration.df.OHS.complex.Eng$obs.perc.upper<-cal.obj.Mod_XGBTREE.1618.OHS.complex.EngTest$data$Upper
calibration.df.OHS.complex.Eng$Model<-"English Test (39 predictors)"

calibration.df.OHS<-rbind(calibration.df.OHS.simple.Eng,
                          calibration.df.OHS.simple.Welsh,
                          calibration.df.OHS.complex.Eng)

#rm(calibration.df.OHS.simple.Eng,calibration.df.OHS.simple.Welsh,calibration.df.OHS.complex.Eng)

library(ggplot2)
ggplot(calibration.df.OHS, aes(x=Pred.Score, y = obs.perc, group = Model)) + 
  geom_line(aes(color=Model))+
  geom_point(aes(color=Model))+
  scale_color_manual(values=c("green", "red", "blue"))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  ylim(0,100) +
  xlim(0,100) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.75, 0.3),
        legend.title = element_blank(),
        legend.text = element_text(size = 7))+
  ggtitle("Calibration plots of best models\npredicting OHS MCID")+
  ylab("Observed Event Percentages")+
  xlab("Model predicted scores")

### Calibration curves: input data *****to restructure

library(ggplot2)
library(ggsci)
## OHS MCID CALIBRATION CURVES (Hips)
attach("H:/MSc_thesis_models/NEW/OHS/HIPS_ML_training_1618_ALL_OHS_curves.RData")
calibration.df.OHS<-calibration.df.OHS
calibration.df.OHS$Model <- factor(calibration.df.OHS$Model, 
                                   levels = c("English Test (39 predictors)", 
                                              "English Test (18 predictors)",
                                              "Welsh Test (18 predictors)"))

## OKS MCID CALIBRATION CURVES (Knees)
attach("H:/MSc_thesis_models/NEW/OKS/KNEES_ML_training_1618_ALL_OKS_curves.RData")
calibration.df.OKS<-calibration.df.OKS

calibration.df.OKS$Model <- factor(calibration.df.VAS.knees$Model, 
                                   levels = c("English Test (39 predictors)", 
                                              "English Test (18 predictors)",
                                              "Welsh Test (18 predictors)"))

## EQ-VAS MCID CALIBRATION CURVES (Hips)
rm(calibration.df.VAS.hips)
attach("H:/MSc_thesis_models/NEW/OHS_VAS/HIPS_ML_training_1618_ALL_VAS_curves.RData")
calibration.df.VAS.hips<-calibration.df.VAS
calibration.df.VAS.hips$Model <- factor(calibration.df.VAS.hips$Model, 
                                        levels = c("English Test (39 predictors)", 
                                                   "English Test (18 predictors)",
                                                   "Welsh Test (18 predictors)"))
## EQ-VAS MCID CALIBRATION CURVES (Knees)
rm(calibration.df.VAS.knees)
attach("H:/MSc_thesis_models/NEW/OKS_VAS/KNEES_ML_training_1618_ALL_OKSVAS_curves.RData")
calibration.df.VAS.knees<-calibration.df.VAS
calibration.df.VAS.knees$Model <- factor(calibration.df.VAS.knees$Model, 
                                         levels = c("English Test (39 predictors)", 
                                                    "English Test (18 predictors)",
                                                    "Welsh Test (18 predictors)"))
### Calibration curves: Welsh vs English Test sets
scale_colour<- c("#ED0000FF","#42B540FF")
plot.calibration.df.OHS<-ggplot2::ggplot(calibration.df.OHS[calibration.df.OHS$Model !="English Test (39 predictors)",], 
                                aes(x=Pred.Score, y = obs.perc, 
                                    ymin = obs.perc.lower, ymax = obs.perc.upper,
                                    group = Model)) + 
  geom_line(aes(color=Model), size = 0.7, alpha = 1)+
  geom_point(aes(color=Model, shape = Model, alpha = 1))+
  scale_colour_manual(values = scale_colour)+
  scale_fill_manual(values = scale_colour)+
  geom_ribbon(aes(fill=Model),alpha=0.3) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "black", size = 0.5)+
  ylim(0,100) +
  xlim(0,100) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  ggtitle("Prediction of OHS MCID\n(Hip dataset)")+
  ylab("Observed Event Percentages")+
  xlab("Model predicted scores")
plot.calibration.df.VAS.hips<-ggplot2::ggplot(calibration.df.VAS.hips[calibration.df.VAS.hips$Model !="English Test (39 predictors)",], 
                                     aes(x=Pred.Score, y = obs.perc, 
                                         ymin = obs.perc.lower, ymax = obs.perc.upper,
                                         group = Model)) +
  geom_line(aes(color=Model), size = 0.7, alpha = 1)+
  geom_point(aes(color=Model,shape = Model, alpha = 1))+
  scale_colour_manual(values = scale_colour)+
  scale_fill_manual(values = scale_colour)+
  geom_ribbon(aes(fill=Model),alpha=0.3) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "black", size = 0.5)+
  ylim(0,100) +
  xlim(0,100) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  ggtitle("Prediction of EQ-VAS MCID\n(Hip dataset)")+
  ylab("Observed Event Percentages")+
  xlab("Model predicted scores")
plot.calibration.df.OKS<-ggplot2::ggplot(calibration.df.OKS[calibration.df.OKS$Model !="English Test (39 predictors)",], 
                                aes(x=Pred.Score, y = obs.perc,
                                    ymin = obs.perc.lower, 
                                    ymax = obs.perc.upper,
                                    group = Model))+
  geom_line(aes(color=Model), size = 0.7, alpha = 1)+
  geom_point(aes(color=Model, shape = Model, alpha = 1))+
  scale_colour_manual(values = scale_colour)+
  scale_fill_manual(values = scale_colour)+
  geom_ribbon(aes(fill=Model),alpha=0.3) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "black", size = 0.5)+
  ylim(0,100) +
  xlim(0,100) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  ggtitle("Prediction of OKS MCID\n(Knee dataset)")+
  ylab("Observed Event Percentages")+
  xlab("Model predicted scores")
plot.calibration.df.VAS.knees<-ggplot2::ggplot(calibration.df.VAS.knees[calibration.df.VAS.knees$Model !="English Test (39 predictors)",], 
                                      aes(x=Pred.Score, y = obs.perc, 
                                          ymin = obs.perc.lower, ymax = obs.perc.upper,
                                          group = Model)) + 
  geom_line(aes(color=Model), size = 0.7, alpha = 1)+
  geom_point(aes(color=Model, shape = Model, alpha = 1))+
  scale_colour_manual(values = scale_colour)+
  scale_fill_manual(values = scale_colour)+
  geom_ribbon(aes(fill=Model),alpha=0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "black", size = 0.5)+
  xlim(0,100) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
        # legend.position = c(0.75, 0.2),
        # legend.title = element_blank(),
        # legend.text = element_text(size = 7)
  )+
  ggtitle("Prediction of EQ-VAS MCID\n(Knee dataset)")+
  ylab("Observed Event Percentages")+
  xlab("Model predicted scores")
ggpubr::ggarrange(plot.calibration.df.OHS,
          plot.calibration.df.VAS.hips,
          plot.calibration.df.OKS,
          plot.calibration.df.VAS.knees,
          ncol = 2, 
          nrow = 2)
## width = 800, height = 800   
#.........................................................................................
#.........................................................................................

# Secondary model: Calibration curves -----------------------------------------------------
#############
### Calibration curves: English 18 vs 39 predictors *****to restructure
scale_colour2<- c("#00468BFF","#ED0000FF")

plot.calibration.df.OHS<-ggplot(calibration.df.OHS[calibration.df.OHS$Model !="Welsh Test (18 predictors)",], 
                                aes(x=Pred.Score, y = obs.perc, 
                                    ymin = obs.perc.lower, ymax = obs.perc.upper,
                                    group = Model)) + 
  geom_line(aes(color=Model), size = 0.7, alpha = 1)+
  geom_point(aes(color=Model, shape = Model, alpha = 1))+
  scale_colour_manual(values = scale_colour2)+
  scale_fill_manual(values = scale_colour2)+
  geom_ribbon(aes(fill=Model),alpha=0.3) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "black", size = 0.5)+
  ylim(0,100) +
  xlim(0,100) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  ggtitle("Prediction of OHS MCID\n(Hip dataset)")+
  ylab("Observed Event Percentages")+
  xlab("Model predicted scores")


library(ggplot2)
plot.calibration.df.VAS.hips<-ggplot(calibration.df.VAS.hips[calibration.df.VAS.hips$Model !="Welsh Test (18 predictors)",], 
                                     aes(x=Pred.Score, y = obs.perc, 
                                         ymin = obs.perc.lower, ymax = obs.perc.upper,
                                         group = Model)) +
  geom_line(aes(color=Model), size = 0.7, alpha = 1)+
  geom_point(aes(color=Model,shape = Model, alpha = 1))+
  scale_colour_manual(values = scale_colour2)+
  scale_fill_manual(values = scale_colour2)+
  geom_ribbon(aes(fill=Model),alpha=0.3) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "black", size = 0.5)+
  ylim(0,100) +
  xlim(0,100) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  ggtitle("Prediction of EQ-VAS MCID\n(Hip dataset)")+
  ylab("Observed Event Percentages")+
  xlab("Model predicted scores")



library(ggplot2)
plot.calibration.df.OKS<-ggplot(calibration.df.OKS[calibration.df.OKS$Model !="Welsh Test (18 predictors)",], 
                                aes(x=Pred.Score, y = obs.perc,
                                    ymin = obs.perc.lower, 
                                    ymax = obs.perc.upper,
                                    group = Model))+
  geom_line(aes(color=Model), size = 0.7, alpha = 1)+
  geom_point(aes(color=Model, shape = Model, alpha = 1))+
  scale_colour_manual(values = scale_colour2)+
  scale_fill_manual(values = scale_colour2)+
  geom_ribbon(aes(fill=Model),alpha=0.3) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "black", size = 0.5)+
  ylim(0,100) +
  xlim(0,100) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  ggtitle("Prediction of OKS MCID\n(Knee dataset)")+
  ylab("Observed Event Percentages")+
  xlab("Model predicted scores")




library(ggplot2)
plot.calibration.df.VAS.knees<-ggplot(calibration.df.VAS.knees[calibration.df.VAS.knees$Model !="Welsh Test (18 predictors)",], 
                                      aes(x=Pred.Score, y = obs.perc, 
                                          ymin = obs.perc.lower, ymax = obs.perc.upper,
                                          group = Model)) + 
  geom_line(aes(color=Model), size = 0.7, alpha = 1)+
  geom_point(aes(color=Model, shape = Model, alpha = 1))+
  scale_colour_manual(values = scale_colour2)+
  scale_fill_manual(values = scale_colour2)+
  geom_ribbon(aes(fill=Model),alpha=0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "black", size = 0.5)+
  xlim(0,100) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"
        # legend.position = c(0.75, 0.2),
        # legend.title = element_blank(),
        # legend.text = element_text(size = 7)
  )+
  ggtitle("Prediction of EQ-VAS MCID\n(Knee dataset)")+
  ylab("Observed Event Percentages")+
  xlab("Model predicted scores")


library(ggpubr)
ggarrange(plot.calibration.df.OHS,
          plot.calibration.df.VAS.hips,
          plot.calibration.df.OKS,
          plot.calibration.df.VAS.knees,
          ncol = 2, 
          nrow = 2)
## width = 800, height = 800






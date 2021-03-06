# MSc-HealthInformatics-thesis
 
Online material for thesis "**Using machine learning models developed with English data to predict improvement in patient-reported outcome measures (PROMs) in a cohort of Welsh patients undergoing joint-replacement surgery**":

- `.gitignore` --> list of files created that have not been uploaded on Github

## **Code**
This folder contains the scripts that have been used for producing the outputs of the thesis: 

###### **Functions**  
- `00_ProjectFunctions.R` --> it contains custom-made functions

###### **Data pre-processing** 
- `01_Preprocess_EnglishHips.R` --> pre-processing of data from the English hip replacement dataset
- `01_Preprocess_EnglishKnees.R` --> pre-processing of data from the English knee replacement dataset
- `01_Preprocess_WelshHips.R` -->  pre-processing of data from the English hip replacement dataset
- `01_Preprocess_WelshKnees.R` -->  pre-processing of data from the English knee replacement dataset

###### **Supervised machine learning**
- `02_ML_main_OHS.R` --> Development/validation of models for predicting post-surgical achievement of OHS MCID in the hip replacement dataset
- `02_ML_main_EQVAS_hips.R` --> Development/validation of models for predicting post-surgical achievement of EQ-VAS MCID in the hip replacement dataset
- `02_ML_main_OKS.R` --> Development/validation of models for predicting post-surgical achievement of OKS MCID in the knee replacement dataset
- `02_ML_main_EQVAS_knees.R` --> Development/validation of models for predicting post-surgical achievement of EQ-VAS MCID in the knee replacement dataset

###### **Plotting**
- `03_Summary_Plots.R` --> Creation of plots for evaluation of models during development and test phases

## supplementary_online_material
- `Data Dictionary - Additional English Predictors.pdf` --> Data dictionary for extra 21 predictors used in supplementary analysis
- `Data Dictionary.pdf` --> Data dictionary for all the variables used in the main analysis
- `Tripod Checklist.pdf` --> Checklist for assessing study's transparency

## supervised-ML-certificates
Certificates of accomplishment of online courses in supervised machine learning. These were followed in order to undertake the training to carry out the MSc thesis project.

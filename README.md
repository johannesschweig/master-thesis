# master-thesis
The code of my master thesis in R and python 3.6 used for data analysis and model building with eye tracking data
No images or data sets are contained
## 01\_data cleaning
data inspection and cleaning for experiment 1
- cleaning.R: contains the cleaning process
- inspection.R: contains inspection of the data and the generation of plots
- stats.R: contains the generation of trial-dependent statistics
- utils.R: contains utility functions used throughout all other scripts in this folder
## 02\_classification
training and evaluation of the classification models on data from experiment 1
- aftermath.R: reads the performance values from crossvalidation+grid search and plots the results
- crossvalidation.py: crossvalidation and grid search for a chosen classifier with the training data
- datasets.R: the creation of the training dataset
- final\_train.py: final training for a chosen classifier, output includes training duration and confusion matrix
- naive\_bayes.py: training and evaluation of the GNB model, output and final model as above
## 03\_correction
training and evaluation of the error correction models on data from experiment 1
- correction\_nonp.R: creation and evaluation of the nonparametric error correction models
- correction\_reg.R: creation of the regression models (output is in correction\_nonp.R)
- crossvaldition.py: crossvalidation and grid search for regression from degree 1 to 20
- final\_train.py: final training of fine tuned 2nd degree regression for X and Y
- inspection.R: inspection of different regressions and plots
- regression tuning.R: exhaustive search for 2nd degree regression for target sizes 1 to 8
## 04\_second-exp
data cleaning and inspection for experiment 2, evaluation of classification and error correction models on data from experiment 2
- classification.py: performs classification on the validation data set
- cleaning.R: contains cleaning process for validation data set
- correction\_nonp.R: evaluation of the nonparametric models on the validation data set
- correction\_reg.R: evaluation of the regression models on the validation data set
- datasets.R: creation of the validation data sets
- inspection.R: inspection of the validation data and plots
- nb\_classification.py: evaluation of the GNB model on the validation data
- stats.R: contains the generation of trial-dependent statistics

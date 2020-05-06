# German Credit Data
 Classifying individuals as good or bad loan, based on their probability to default.


The German credit score data are downloadable from
http://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)

Variable Selection Logistic.R

Random sample a training dataset containing 70% of data, remaining 30% is used for testing and calulating out of sample performance.

Logistic model is fit on data and best model is selected using AIC, BIC, and LASSO variable selection.

ROC is plotted, AUC is reported


GAMneuralg.R

GAM and Neural Net is fitted on german data set, out of sample performance is reported.

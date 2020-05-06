credit.data <- read.table("german.data")
library(dplyr)
library(MASS)
library(DT)
library(leaps)
library(glmnet)
library(PerformanceAnalytics)
library(GGally)
library(tidyr)

colnames(credit.data)=c("chk_acct","duration","credit_his","purpose","amount","saving_acct","present_emp","installment_rate","sex","other_debtor","present_resid","property","age","other_install","housing","n_credits","job","n_people","telephone","foreign","response")

#orginal response coding 1= good, 2 = bad
#we need 0 = good, 1 = bad

credit.data$response = credit.data$response - 1
str(credit.data)


credit.data$SEX<- as.factor(credit.data$SEX)



index <- sample(nrow(credit.data),nrow(credit.data)*0.70)
germandata.train = credit.data[index,]
germandata.test = credit.data[-index,]

creditcost <- function(observed, predicted){
  weight1 = 5
  weight0 = 1
  c1 = (observed==1)&(predicted == 0) #logical vector - true if actual 1 but predict 0
  c0 = (observed==0)&(predicted == 1) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}

library(mgcv)
### GAM
## Create a formula for a model with a large number of variables:
germandata.gam <- gam(as.factor(response)~chk_acct+s(duration)+credit_his+purpose+s(amount)+saving_acct+present_emp+installment_rate+sex+other_debtor+present_resid+property
                      +s(age)+other_install+housing+n_credits+telephone+foreign , family=binomial,data=germandata.train)
summary(germandata.gam)
plot(germandata.gam, shade=TRUE)

germandata1.gam <- gam(as.factor(response)~chk_acct+(duration)+credit_his+purpose+s(amount)+saving_acct+present_emp+installment_rate+sex+other_debtor+present_resid+property
                       +(age)+other_install+housing+n_credits+telephone+foreign , family=binomial,data=germandata.train)
summary(germandata1.gam)


AIC(germandata1.gam)
BIC(germandata1.gam)
germandata1.gam$deviance

pcut.gam <- (1/6)
prob.gam.in<-predict(germandata1.gam,germandata.train,type="response")
pred.gam.in<-(prob.gam.in>=pcut.gam)*1
table(germandata.train$response,pred.gam.in,dnn=c("Observed","Predicted"))
#MR
mean(ifelse(germandata.train$response != pred.gam.in, 1, 0))
#Cost assocaited with MR
creditcost(germandata.train$response, pred.gam.in)

#Out-of-sample performance########
prob.gam.out<-predict(germandata.gam,germandata.test,type="response")
pred.gam.out<-(prob.gam.out>=pcut)*1
table(germandata.test$response,pred.gam.out,dnn=c("Observed","Predicted"))

#MR
mean(ifelse(germandata.test$response != pred.gam.out, 1, 0))
#Cost assocaited with MR
creditcost(germandata.test$response, pred.gam.out)

#####Neural Net####
library(caret)
library(NeuralNetTools)
par(mfrow=c(1,1))
germandata.nnet <- train(as.factor(response)~., data=germandata.train,method="nnet",na.action=na.exclude,maxit=300,act.fct="logistic",linear.output=FALSE,learningrate=0.1)
plot(germandata.nnet)
print(germandata.nnet)
plotnet(germandata.nnet$finalModel, y_names = "response")
title("Graphical Representation of our Neural Network")

pcut=1/6
#In sample
prob.nnet= predict(germandata.nnet,type='prob')
pred.nnet = as.numeric(prob.nnet[,2] >=pcut)
table(germandata.train$response,pred.nnet, dnn=c("Observed","Predicted"))
#MR
mean(ifelse(germandata.train$response != pred.nnet, 1, 0))
#Costfunction
creditcost(germandata.train$response, pred.nnet)


#Out of sample
prob.nnet.test= predict(germandata.nnet,germandata.test,type='prob')
pred.nnet.test = as.numeric(prob.nnet.test[,2] > pcut)
table(germandata.test$response,pred.nnet.test, dnn=c("Observed","Predicted"))
##MR
mean(ifelse(germandata.test$response != pred.nnet.test, 1, 0))
#Costfunction
creditcost(germandata.test$response, pred.nnet.test)



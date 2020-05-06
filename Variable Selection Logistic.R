library(knitr)
library(dplyr)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(GGally)
library(ggplot2)
library(caret)
library(glmnet)
library(boot)
library(verification)

german_credit<-read.table("german.data")
psych::describe(german_credit)
colnames(german_credit)=c("chk_acct","duration","credit_his","purpose","amount","saving_acct","present_emp","installment_rate","sex","other_debtor","present_resid","property","age","other_install","housing","n_credits","job","n_people","telephone","foreign","response")

#orginal response coding 1= good, 2 = bad
#we need 0 = good, 1 = bad
german_credit$response = german_credit$response - 1

str(german_credit)
#converting response to factor

german_credit$response <- as.factor(german_credit$response)

summary(german_credit)
#plotting age vs response
ggplot(melt(german_credit[,c(13,21)]), aes(x = variable, y = value, fill = response)) + 
geom_boxplot()+ xlab("response") + ylab("age")

#count vs installment rate

ggplot(german_credit,aes(x=installment_rate, fill=response)) +
  geom_histogram()
ggplot(melt(german_credit[,c(8,21)]), aes(x = variable, y = value, fill = response)) + 
  geom_boxplot() + xlab("installment_rate")

#duration vs response
test.m = german_credit[,c(2,5,8,13,16,18,21)]
test.m$response <- as.numeric(test.m$response)
ggplot(melt(german_credit[,c(2,21)]), aes(x = variable, y = value, fill = response)) + geom_boxplot() + xlab("response") + ylab("duration")

#amount vs response
ggplot(melt(german_credit[,c(5,21)]), aes(x = variable, y = value, fill = response)) + 
  geom_boxplot() + xlab("response") + ylab("amount")



#categorical variables
#chi square



#foreign
ggplot(german_credit, aes(foreign, ..count..)) + 
geom_bar(aes(fill = response), position = "dodge") 

table.edu <- table(german_credit$foreign, german_credit$response)
chisq.test(table.edu)

#chk_acct
ggplot(german_credit, aes(chk_acct, ..count..)) + 
geom_bar(aes(fill = response), position = "dodge")

table.edu1 <- table(german_credit$chk_acct, german_credit$response)
chisq.test(table.edu1)

#credit_his
ggplot(german_credit, aes(credit_his, ..count..)) + 
geom_bar(aes(fill = response), position = "dodge") 

table.edu2 <- table(german_credit$credit_his, german_credit$response)
chisq.test(table.edu2)

#other_debtor
ggplot(german_credit, aes(other_debtor, ..count..)) + 
geom_bar(aes(fill = response), position = "dodge") 

table.edu3 <- table(german_credit$other_debtor, german_credit$response)
chisq.test(table.edu3)

#purpose
ggplot(german_credit, aes(purpose, ..count..)) + 
geom_bar(aes(fill = response), position = "dodge")

table.edu4 <- table(german_credit$purpose, german_credit$response)
chisq.test(table.edu4)

#saving_acct
ggplot(german_credit, aes(saving_acct, ..count..)) + 
geom_bar(aes(fill = response), position = "dodge") 

table.edu5 <- table(german_credit$saving_acct, german_credit$response)
chisq.test(table.edu5)

#sex
ggplot(german_credit, aes(sex, ..count..)) + 
geom_bar(aes(fill = response), position = "dodge") 

table.edu6 <- table(german_credit$sex, german_credit$response)
chisq.test(table.edu6)

#other_install
ggplot(german_credit, aes(other_install, ..count..)) + 
geom_bar(aes(fill = response), position = "dodge") 

table.edu7 <- table(german_credit$other_install, german_credit$response)
chisq.test(table.edu7)



#split data 70-30
set.seed(13474715)
index <- sample(nrow(german_credit),nrow(german_credit)*0.70)
german_train = german_credit[index,]
german_test = german_credit[-index,]
#logistic reg
credit.glm0<- glm(response~., family=binomial, data=german_train)
summary(credit.glm0)

credit.glm0$deviance
AIC(credit.glm0)
BIC(credit.glm0)


#in sample prediction
pred.glm0.train<- predict(credit.glm0, type="response")

library(ROCR)

pred <- prediction(pred.glm0.train, german_train$response)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

unlist(slot(performance(pred, "auc"), "y.values"))

#null model
model.null <- glm(response ~ 1, family = binomial ,data = german_train)
AIC(model.null)

#stepwise variable selection 
#AIC method
#forward
credit.glm.f<-step(model.null, scope= list(lower = model.null, upper = credit.glm0), trace=0, direction = "forward")
summary(credit.glm.f)
AIC(credit.glm.f)
#backward
credit.glm.b <- step(credit.glm0, scope= list(lower = model.null, upper = credit.glm0), trace=0, direction = "backward")
summary(credit.glm.b)
AIC(credit.glm.b)


#BIC method
#forward
step.BIC.f <- step(model.null, scope = list(lower = model.null, upper = credit.glm0), direction = "forward", k = log(nrow(german_train)))
AIC(step.BIC.f)
#backward
step.BIC.b <- step(credit.glm0, scope = list(lower = model.null, upper = credit.glm0), direction = "backward", k = log(nrow(german_train)))
AIC(step.BIC.b)


#in-sample prediction
#aic models
pred.glm.f<- predict(credit.glm.f, type="response")
pred.glm.b<- predict(credit.glm.b, type="response")
#bicmodels
pred.glm.bf<- predict(step.BIC.f, type="response")
pred.glm.bb<- predict(step.BIC.b, type="response")

#AUC
#forward model using AIC
pred1 <- prediction(pred.glm.f, german_train$response)
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1, colorize=TRUE)
unlist(slot(performance(pred1, "auc"), "y.values"))
#backward model using AIC
pred2 <- prediction(pred.glm.b, german_train$response)
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, colorize=TRUE)
unlist(slot(performance(pred2, "auc"), "y.values"))
#forward model using BIC
pred3 <- prediction(pred.glm.bf, german_train$response)
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3, colorize=TRUE)
unlist(slot(performance(pred3, "auc"), "y.values"))
#backward model using BIC
pred4 <- prediction(pred.glm.bb, german_train$response)
perf4 <- performance(pred4, "tpr", "fpr")
plot(perf4, colorize=TRUE)
unlist(slot(performance(pred4, "auc"), "y.values"))

#we can see that backward variable selection using AIC has the lowest AIC value and also has maximum area under the curve.

#final model is 
credit.glm.b

#misclassification rate table


#define a cost rate function

costfunc = function(obs, pred.p, pcut){
  weight1 = 5   # define the weight for "true=1 but pred=0" (FN)
  weight0 = 1    # define the weight for "true=0 but pred=1" (FP)
  c1 = (obs==1)&(pred.p<pcut)    # count for "true=1 but pred=0"   (FN)
  c0 = (obs==0)&(pred.p>=pcut)   # count for "true=0 but pred=1"   (FP)
  cost = mean(weight1*c1 + weight0*c0)  # misclassification with weight
  return(cost)
}
  p.seq = seq(0.01, 1, 0.01) 
  
  cost = rep(0, length(p.seq))  
  for(i in 1:length(p.seq)){ 
    cost[i] = costfunc(obs = german_train$response, pred.p = pred.glm.b, pcut = p.seq[i])  
  }
  
  
plot(p.seq, cost)
  
optimal.pcut.glm0 = 0.17


class.glm0.train.opt<- (pred.glm.b>optimal.pcut.glm0)*1
table(german_train$response, class.glm0.train.opt, dnn = c("True", "Predicted"))


MR<- mean(german_train$response!= class.glm0.train.opt)

#out-sample
pred.glm0.test<- predict(credit.glm.b, newdata = german_test[,-which(names(german_test)=='response')], type="response")

#AUC
pred5 <- prediction(pred.glm0.test, german_test$response)
perf5 <- performance(pred5, "tpr", "fpr")
plot(perf5, colorize=TRUE)
unlist(slot(performance(pred5, "auc"), "y.values"))

#MR

class.glm0.test.opt<- (pred.glm0.test>optimal.pcut.glm0)*1
table(german_test$response, class.glm0.test.opt, dnn = c("True", "Predicted"))

MR1<- mean(german_test$response!= class.glm0.test.opt)

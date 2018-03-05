#Kickstarter Project, Take III

library(glmnet)
library(gam)
library(MASS)
library(data.table)
library(ggplot2)
library(xtable)
library(plyr)
library(pROC)
library(gam)

setwd("../Desktop")

set.seed(223)

ks714<-fread("ks714_v1.csv")

approxAcc<-function(yhat,y,cutoff){
  pred<-(yhat>=cutoff)*1
  confmat<-table(pred,y)
  acc<-sum(diag(confmat))/length(y)
  return(acc)
}

categoryNorm<-function(X,catVar,data){
  mean_SD<-aggregate(as.formula(paste0(X,"~",catVar)),data,function(x)c(mean(x),sd(x)))
  mean_SD<-data.frame(mean_SD[,1],mean_SD[,2][,1],mean_SD[,2][,2])
  colnames(mean_SD)<-c(catVar,"mean","sd")
  data_meanSD<-join(data,mean_SD)[,c("mean","sd")]
  norm_X<-(data[,X]-data_meanSD[,1])/data_meanSD[,2]
  return(norm_X)
}

##PART I: Choice of Categories
#We keep the top 5 categories

ks714$category_combo<-factor(paste0(ks714$main_category,":",ks714$category))

top5cats<-names(tail(sort(table(ks714$category_combo)),5))

#Keep the top 5, drop the combo column, subset the data
ks714L<-ks714[ks714$category_combo%in%top5cats,-21]

#Also drop NL/NZ for now, my change later
ks714L<-ks714L[grepl("N[ZL]",ks714L$country)==FALSE,]

#Our remaining categories are: Art, Documentaries, Food, Music and Product Design

#Place approximately 40% of the data in the test set:
test_ids<-sample(1:nrow(ks714L),floor(.4*nrow(ks714L)))

#Part II: Exploratory Data Analysis
#We have four essential predictors: goal, duration, country, category
#We want to examine their effects

##i. CATEGORY##

##ii. GOAL##

#Our measure is in real USD dollars. So all currencies are accounted for.
#What does the data look like?
plot(sort(ks714L$usd_goal_real))
#One big outlier, one MASSIVE outlier. Data needs to be transformed. Consider log
#Even without the outlier, the data is incredibly skewed
hist(ks714L$usd_goal_real[ks714L$usd_goal_real<=250000],breaks=25)

#A log transform is common for large numeric measures. It would work well here for the money as well.

ks714L$log_goal<-log(ks714L$usd_goal_real)

#Let's normalize the log goal, it will help distinguish what will succed and not succeed.

ks714L$norm_goal<-categoryNorm("log_goal","category",ks714L)

#What does the norm goal look like across project categories?

p <- ggplot(ks714L, aes(x=category, y=norm_goal,fill=factor(outcome))) + 
  geom_violin(position=position_dodge(1))
p

#Some interesting patterns here, mainly that food has its successes at much lower amounts.
#Music is more likely to be successful at higher amounts.

##iii. DURATION##

#We'll also normalize duration within category

ks714L$norm_duration<-categoryNorm("duration","category",ks714L)

#What does the duration look against outcome?

plot(x=ks714L$norm_duration,y=ks714L$outcome)
lines(smooth.spline(ks714L$norm_duration,ks714L$outcome, spar = 1.1), col = "blue",lwd=2)
abline(h=mean(ks714L$outcome),col="red",lwd=2)

#Duration looks to be a negative effect. The longer the project, the less likely to succeed.

#How does duration look versus the normalized goal?
plot(x=ks714L$norm_duration,y=ks714L$norm_goal)
lines(smooth.spline(ks714L$norm_duration,ks714L$norm_goal, spar = 1.3), col = "blue",lwd=2)

#Slight linear effect, probably not worth taking into consideration.

##Alternative analysis, dollars per day.##

ks714L$DPD<-log(ks714L$usd_goal_real/ks714L$duration)
ks714L$norm_DPD<-categoryNorm("DPD","category",ks714L)

p <- ggplot(ks714L, aes(x=category, y=norm_duration,fill=factor(outcome))) + 
  geom_violin(position=position_dodge(1))
p

##iv. COUNTRY##

p <- ggplot(ks714L, aes(x=country, y=norm_goal,fill=factor(outcome))) + 
  geom_violin(position=position_dodge(1))
p

#US is more likely to fund 'extravagant' projects.

#One thing to consider: with the data dropped to 2K, consider removing NL/NZ from data.

#Part III: Initial model fitting
#We need to split the data into a training and test set:

ks714LTR<-ks714L[-test_ids,c("outcome","norm_goal","norm_duration","category","country")]
ks714LTE<-ks714L[test_ids,c("outcome","norm_goal","norm_duration","category","country")]

#See the prediction accuracy rate for the simplest model, predict success based on norm_goal

model_1<-glm(outcome~norm_goal,ks714LTR,family=binomial())
ks714LTE$m1pred<-predict(model_1,data.frame(norm_goal=ks714LTE$norm_goal),type="response")

m1_roc<-roc(outcome~m1pred,ks714LTE)
plot(m1_roc)

m1_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714LTE$m1pred,ks714LTE$outcome,x))

#.5776 AUC, not very strong.

model_2<-glm(outcome~norm_goal+category,ks714LTR,family=binomial())
ks714LTE$m2pred<-predict(model_2,data.frame(norm_goal=ks714LTE$norm_goal,category=ks714LTE$category),type="response")

m2_roc<-roc(outcome~m2pred,ks714LTE)
plot(m2_roc)

#.6691 AUC, an improvement

m2_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714LTE$m2pred,ks714LTE$outcome,x))

#Now try adding duration and country

model_3<-glm(outcome~norm_goal+norm_duration+category+country,ks714LTR,family=binomial())
ks714LTE$m3pred<-predict(model_3,data.frame(ks714LTE[,2:5]),type="response")

m3_roc<-roc(outcome~m3pred,ks714LTE)
plot(m3_roc)

#.6597, does not help.

m3_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714LTE$m3pred,ks714LTE$outcome,x))

#Next model, second order effects with lasso

ksTR_data_matrix<-sparse.model.matrix(outcome~-1+(norm_goal+I(norm_goal^2)+norm_duration+I(norm_duration^2)+category+country)^2,data=ks714LTR)
ksTE_data_matrix<-sparse.model.matrix(outcome~-1+(norm_goal+I(norm_goal^2)+norm_duration+I(norm_duration^2)+category+country)^2,data=ks714LTE)

plot(ks_lasso_fit,xvar="dev")

ks_lasso_cv<-cv.glmnet(ksTR_data_matrix,ks714LTR$outcome,family="binomial",type.measure="auc")

ks_lasso_fit<-glmnet(ksTE_data_matrix, ks714LTE$outcome,lambda=ks_lasso_cv$lambda.min,family = "binomial")

#Examine the results

plot(ks_lasso_cv)

plot(ks_lasso_cv$glmnet.fit,"lambda")

ks714LTE$m4pred<-predict(ks_lasso_fit,newx=ksTE_data_matrix,type="response",s=ks_lasso_cv$lambda.min)

m4_roc<-roc(outcome~m4pred,ks714LTE)
plot(m4_roc)

#A bit better at .6882. Similar effects to before.

m4_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714LTE$m4pred,ks714LTE$outcome,x))

#Here's a plot of the accuracy across each model for cutoff value

matplot(x=seq(.01,.99,.01),do.call(cbind,list(m1_acc,m2_acc,m3_acc,m4_acc)),type='l')

#At 0.5, we'll always get 80% accuracy it appears
#We have to consider which metric we want to MAXIMIZE. AUC is kind of a 'catch-all'.
#Model 4 is clearly the best model for prediction so far. But can we improve it?

#Let's examine some new variables
ks714NL<-read.csv("ks714NLP2.csv")
ks714NL$cluster<-factor(ks714NL$cluster+1)

#Also, re drop NL and NZ for now

ks714NL<-ks714NL[grepl("N[ZL]",ks714NL$country)==FALSE,]

#We now re-define the categories by adding the 'Potato Salad' factor
#Example: look at the log hours until launch with potato salad included
boxplot(log_goal~category2,ks714NL)

#We renormalize based on category 2

ks714NL$norm_goal<-categoryNorm("log_goal","category2",ks714NL)
ks714NL$normhours<-categoryNorm("loghours","category2",ks714NL)
ks714NL$norm_duration<-categoryNorm("duration","category2",ks714NL)
ks714NL$normtext<-categoryNorm("textScore","category2",ks714NL)

#Let's examine a few predictor relationships
p <- ggplot(ks714NL, aes(x=category2, y=normhours,fill=factor(outcome))) + 
  geom_violin(position=position_dodge(1))
p

qplot(x=ks714NL$normhours,y=ks714NL$norm_goal,color=ks714NL$category2)

#Model 5

ks714NLTR<-ks714NL[-test_ids,c("outcome","norm_goal","normhours","category2","normtext","duration","country")]
ks714NLTE<-ks714NL[test_ids,c("outcome","norm_goal","normhours","category2","normtext","duration","country")]

model_5<-glm(outcome~(norm_goal+I(norm_goal^2)+normhours+I(normhours^2))*category2,ks714NLTR,family=binomial())
ks714NLTE$m5pred<-predict(model_5,data.frame(norm_goal=ks714NLTE$norm_goal,normhours=ks714NLTE$normhours,category2=ks714NLTE$category2),type="response")

m5_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714NLTE$m5pred,ks714NLTE$outcome,x))

m5_roc<-roc(outcome~m5pred,ks714NLTE)
#AUC: .7368, a big improvement!

plot(seq(.01,.99,.01),m5_acc,type='l')

#Model 6: Lasso with auxiliary features

ksTR_data_matrix<-sparse.model.matrix(outcome~category2*(norm_goal+I(norm_goal^2)+normhours+I(normhours^2)+normtext+I(normtext^2))^2,data=ks714NLTR)
ksTE_data_matrix<-sparse.model.matrix(outcome~category2*(norm_goal+I(norm_goal^2)+normhours+I(normhours^2)+normtext+I(normtext^2))^2,data=ks714NLTE)

ks_lasso_cv<-cv.glmnet(ksTR_data_matrix,ks714NLTR$outcome,family="binomial",type.measure="auc")

ks_lasso_fit<-glmnet(ksTE_data_matrix, ks714NLTE$outcome,lambda=ks_lasso_cv$lambda.min,family = "binomial")

ks714NLTE$m6pred<-predict(ks_lasso_fit,newx=ksTE_data_matrix,type="response",s=ks_lasso_cv$lambda.min)

m6_roc<-roc(outcome~as.vector(m6pred),ks714NLTE)
#AUC: .7819. Even better, the fit is starting to become solid

m6_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714NLTE$m6pred,ks714NLTE$outcome,x))

#CURRENT TABLE:
#MODEL 1: AUC .6148
#MODEL 2: AUC .6691
#MODEL 3: AUC .6597
#MODEL 4: AUC .6882
#MODEL 5: AUC .7368
#MODEL 6: AUC .7819

#Accuracy plot:
matplot(x=seq(.01,.99,.01),do.call(cbind,list(m1_acc,m2_acc,m3_acc,m4_acc,m5_acc,m6_acc)),type='l')

#Models 1-4: approx 50% accuracy at mean cutoff
#Models 5-6: approx 65% accuracy at mean cutoff

#How has the roc improved?

ROC_list<-list(m1_roc,m2_roc,m3_roc,m4_roc,m5_roc,m6_roc)

sapply(1:length(ROC_list),function(x)lines(ROC_list[[x]],lty=x))

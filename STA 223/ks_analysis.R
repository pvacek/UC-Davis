#Kickstarter Project, Take II

library(glmnet)
library(MASS)
library(data.table)
library(ggplot2)
library(xtable)
library(plyr)
library(lawstat)
library(pROC)

setwd("../Desktop")

ks714<-fread("ks714_v1.csv")

###
#Part I:
#Sparse category removal.
###
#For each category count that is below 20, drop the data.

ks714$category_combo<-factor(paste0(ks714$main_category,":",ks714$category))

nlevels(ks714$category_combo)
#Without dropping we currently have 163 levels

combo_table<-table(ks714$category_combo)

#We now drop the sparse categories and work with the 'lean' July 2014 data (ks714L, L=lean)

ks714L<-ks714[ks714$category_combo %in% names(which(combo_table>=20)),]
ks714L$category_combo<-factor(ks714L$category_combo)

#How many observations did we drop?

1-nrow(ks714L)/nrow(ks714)

#Roughly 5%.

#What are the top 5 most successful categories?

cat_succ_rate<-prop.table(cat_succ_rate,margin=1)[,2]
top5<-tail(sort(cat_succ_rate),n=5)
names(top5)<-gsub("^.+:","",names(top5))
barplot(top5)

#What are the bottom 5?

bot5<-head(sort(cat_succ_rate),n=5)
names(bot5)<-gsub("^.+:","",names(bot5))
barplot(bot5)

#Continue with further exploration here.

###
#Part II:
#Analyzing the goal
###

#Note that the USD goal real is significantly skewed, we'll work with log dollars.

ks714L$log_goal<-log(ks714L$usd_goal_real)

goal_meanSD<-aggregate(log_goal~category_combo,ks714L,function(x)c(mean(x),sd(x)))
goal_meanSD<-data.frame(goal_meanSD[,1],goal_meanSD[,2][,1],goal_meanSD[,2][,2])
colnames(goal_meanSD)<-c("category_combo","mean","sd")
ks714L_meanSD<-join(ks714L,goal_meanSD)[,c("mean","sd")]
ks714L$norm_goal<-unlist((ks714L$log_goal-ks714L_meanSD[,1])/ks714L_meanSD[,2])

#Now -- let's analyze the normalized goal. How well does it stack up with the outcome?
plot(x=ks714L$norm_goal,y=ks714L$outcome)
lines(smooth.spline(ks714L$norm_goal,ks714L$outcome, spar = 1.2), col = "blue",lwd=2)

#Pretty well, there may be some abnormal observations though

###
#Part III:
#Analyzing the two other features: duration and country
###

##Duration##

#Note the following: I. Duration has sparse categories, potentially better to do by five day duration instead.

ks714L$duration2<-as.integer(cut(ks714L$duration,seq(0,60,5),1:12))

duration_succ_rate<-prop.table(table(ks714L$duration,ks714L$outcome),margin=1)[,2]

#What does the success rate pattern for duration look like?

plot(x=sort(unique(ks714L$duration)),y=duration_succ_rate,xlab="Duration (days)",
     ylab="Success Rate",main="Project Success Rate by Duration")
lines(smooth.spline(sort(unique(ks714L$duration)),duration_succ_rate, spar = 0.9), col = "blue",lwd=2)
abline(h=mean(ks714L$outcome),col="red",lwd=2)

#It could be potentially quadratic, but it should be noted that the counts in these categories is sparse.

##Country##

country_succ_rate<-prop.table(table(ks714L$country,ks714L$outcome),margin=1)[,2]

#Which countries are the most successful?

barplot(sort(country_succ_rate),xlab="Country",ylab="Success Rate",
        main="Which countries have the best success rate?")

###
#Part IV:
#Building the initial model
###

##Model I: Standalone Goal##

model_1<-glm(outcome~norm_goal,ks714L,family=binomial())

#Analyzing goodness of fit

#Deviance vs. Pearson residuals

m1_rP<-residuals(model_1,"pearson")
m1_rD<-residuals(model_1,"deviance")
m1_yhat<-model_1$fitted.values

par(mfrow=c(1,2))

plot(x=m1_yhat,y=m1_rP,xlab="Fitted Values",ylab="Residuals",main="Pearson")
abline(h=0,col="red",lty=2)
lines(smooth.spline(m1_yhat,m1_rP, spar = 1.2), col = "blue",lwd=2)
plot(x=m1_yhat,y=m1_rD,xlab="Fitted Values",ylab="Residuals",main="Deviance")
abline(h=0,col="red",lty=2)
lines(smooth.spline(m1_yhat,m1_rD, spar = 1.2), col = "blue",lwd=2)

par(mfrow=c(1,1))

#There may be some form of significant pattern, both residuals seem to have negative runs at the tails.

#Which set has more outliers?

boxplot(data.frame("Pearson"=m1_rP,"Deviance"=m1_rD),ylab="Residuals",main="Deviance vs. Pearson Residuals")

#We now do the runs tests

runs.test(m1_rP)
runs.test(m1_rD)

#Both p-values are not small but not 1 either. There may be a pattern but it isn't obvious enough.

m1_preds<-predict(model_1,type="response")

m1_roc<-roc(ks714L$outcome~m1_preds)
plot(m2_roc)

#Leverage vs. Cook's Distance

m1_lev<-hatvalues(model_1)
m1_cooks<-cooks.distance(model_1)

par(mfrow=c(1,2))
plot(names(m1_lev), m1_lev, xlab="Index", type="h",main="Leverage")
points(names(m1_lev), m1_lev)
abline(h=2*length(model_1$coefficients)/nrow(ks714L),col=2,lwd=2,lty=2)
plot(m1_cooks, main="Cook's Distance", ylab="Cook's Distance")
par(mfrow=c(1,1))

##Model II: Full Model##

#Conveniently, the category most similar to the baseline is 'Art', which is already set as the baseline here.
#We are now considering: outcome~norm_goal+category+duration+country

model_2<-glm(outcome~norm_goal+category_combo,ks714L,family=binomial())

#Deviance vs. Pearson residuals

m2_rP<-residuals(model_2,"pearson")
m2_rD<-residuals(model_2,"deviance")
m2_yhat<-model_2$fitted.values

par(mfrow=c(1,2))

plot(x=m2_yhat,y=m2_rP,xlab="Fitted Values",ylab="Residuals",main="Pearson")
abline(h=0,col="red",lty=2)
lines(smooth.spline(m2_yhat,m2_rP, spar = 1.2), col = "blue",lwd=2)
plot(x=m2_yhat,y=m2_rD,xlab="Fitted Values",ylab="Residuals",main="Deviance")
abline(h=0,col="red",lty=2)
lines(smooth.spline(m2_yhat,m2_rD, spar = 1.2), col = "blue",lwd=2)

par(mfrow=c(1,1))

#The pattern appears worse now. Fit isn't as good as before due to potential overfitting.

#Which set has more outliers?

boxplot(data.frame("Pearson"=m2_rP,"Deviance"=m2_rD),ylab="Residuals",main="Deviance vs. Pearson Residuals")

#Once again it's Pearson.

#We now do the runs tests

runs.test(m2_rP)
runs.test(m2_rD)

#Runs tests now have smaller p-values -- pattern is more significant.

#What about the prediction? AUC?

m2_preds<-predict(model_2,type="response")

m2_roc<-roc(ks714L$outcome~m2_preds)
plot(m2_roc)

#Leverage/Cooks distance

m2_lev<-hatvalues(model_2)
m2_cooks<-cooks.distance(model_2)

par(mfrow=c(1,2))
plot(names(m2_lev), m2_lev, xlab="Index", type="h",main="Leverage")
points(names(m2_lev), m2_lev)
abline(h=2*length(model_2$coefficients)/nrow(ks714L),col=2,lwd=2,lty=2)
plot(m2_cooks, main="Cook's Distance", ylab="Cook's Distance")
par(mfrow=c(1,1))

##Model III: Full Model##

ks714L$country<-relevel(factor(ks714L$country),"US")

#We also need to 'center' duration if we want to examine its quadratic effect

ks714L$duration3<-ks714L$duration-mean(ks714L$duration)

model_3<-glm(outcome~norm_goal+category_combo+duration3+I(duration3^2)+country,ks714L,family=binomial())

#Deviance vs. Pearson residuals

m3_rP<-residuals(model_3,"pearson")
m3_rD<-residuals(model_3,"deviance")
m3_yhat<-model_3$fitted.values

par(mfrow=c(1,2))

plot(x=m3_yhat,y=m3_rP,xlab="Fitted Values",ylab="Residuals",main="Pearson")
abline(h=0,col="red",lty=2)
lines(smooth.spline(m3_yhat,m3_rP, spar = 1.2), col = "blue",lwd=2)
plot(x=m3_yhat,y=m3_rD,xlab="Fitted Values",ylab="Residuals",main="Deviance")
abline(h=0,col="red",lty=2)
lines(smooth.spline(m3_yhat,m3_rD, spar = 1.2), col = "blue",lwd=2)

par(mfrow=c(1,1))

#The pattern appears worse now. Fit isn't as good as before due to potential overfitting.

#Which set has more outliers?

boxplot(data.frame("Pearson"=m3_rP,"Deviance"=m3_rD),ylab="Residuals",main="Deviance vs. Pearson Residuals")

#Once again it's Pearson.

#We now do the runs tests

runs.test(m3_rP)
runs.test(m3_rD)

#Runs test is getting bad now...residuals are becoming quite systematic.
#ROC:

m3_preds<-predict(model_3,type="response")

m3_roc<-roc(ks714L$outcome~m3_preds)
plot(m3_roc)

#Leverage/Cooks

m3_lev<-hatvalues(model_3)
m3_cooks<-cooks.distance(model_3)

par(mfrow=c(1,2))
plot(names(m3_lev), m3_lev, xlab="Index", type="h",main="Leverage")
points(names(m3_lev), m3_lev)
abline(h=2*length(model_3$coefficients)/nrow(ks714L),col=2,lwd=2,lty=2)
plot(m3_cooks, main="Cook's Distance", ylab="Cook's Distance")
par(mfrow=c(1,1))

####################################
###We now attempt to do the Lasso###
####################################

##Model IV: Full with Lasso##

#Create a one-hot data matrix (remove the redundant intercept)

ks_data_matrix<-sparse.model.matrix(outcome~norm_goal+category_combo+duration3+I(duration3^2)+country,data=ks714L)[,-1]

#Fit the lasso model, perform cross validation

ks_lasso_fit<-glmnet(ks_data_matrix, ks714L$outcome,family = "binomial")

plot(ks_lasso_fit,xvar="dev")

ks_lasso_cv<-cv.glmnet(ks_data_matrix,ks714L$outcome,family="binomial",type.measure="auc")

#Examine the results

plot(ks_lasso_cv)

plot(ks_lasso_cv$glmnet.fit,"lambda")

#We can choose between:
#1. The minimal lambda model: 93/110 categories kept.
#2. The 1 S.E. model: 57/110 categories kept.
#Art is the '111th category' but it's the baseline.

ks_cats_1se<-coef(ks_lasso_cv,s="lambda.1se")
ks_cats_min<-coef(ks_lasso_cv,s="lambda.min")

#For now, use 1 S.E. model (it is more parsimonious) (potentially change this later)

ks_lasso_preds<-as.vector(predict(ks_lasso_fit,newx=ks_data_matrix,type="response",s=ks_lasso_cv$lambda.1se))

lasso_roc<-roc(ks714L$outcome~ks_lasso_preds)

lasso_roc

#To-do with this model: measure goodness of fit somehow (idk how to do this yet)

###
#Part V:
#Modeling succes within specific categories
###

#Initial question: Which categories were easiest to predict? Hardest to predict?
#For now, use the lasso model.

ks714L$predict<-(ks_lasso_preds>=mean(ks714L$outcome))*1
ks714L$match<-(ks714L$predict==ks714L$outcome)*1
ks714L$found<-(ks714L$outcome==1 & ks714L$predict ==1)*1

ks_succ_cat<-aggregate(match~category_combo,ks714L,function(x)c(mean(x),sum(x),length(x)))

ks_recall<-aggregate(found~category_combo,subset(ks714L,outcome==1),sum)

ks_succ_cat<-join(ks_succ_cat,ks_recall)

ks_succ_cat$found[apply(is.na(ks_succ_cat),1,any)]<-0

n_succ<-table(subset(ks714L,outcome==1)$category_combo)

qplot(x=ks_succ_cat[,2][,1],y=ks_succ_cat[,2][,3],xlab="Accuracy",ylab="# of projects")

qplot(x=cat_succ_rate,y=ks_succ_cat[,2][,1],xlab="Success Rate",ylab="Accuracy Rate",
      size=ks_succ_cat[,2][,3],ylim=c(0,1))

qplot(x=ks_succ_cat$found,y=as.vector(n_succ),xlab="# Retrieved",ylab="# Successful",
      size=ks_succ_cat[,2][,3])+geom_abline(intercept = -1,slope=1,col="red",lwd=1)

#IDEA 1:
#Select the 5 categories that we're underperforming on
#This could be reduced to three if necessary

#Top 5: Food, Music, Art, Design, Documentary

#IDEA 2:
#Select the top 5 successful categories (this is not the best idea because a lot of these categories are small)

#Here are some ideas for idea 2:
names(tail(sort(cat_succ_rate),10))

five_cats<-names(tail(n_succ[order(as.vector(n_succ)-ks_succ_cat$found)],5))

ks714C<-ks714L[ks714L$category_combo%in%five_cats,]
ks714C$category<-factor(ks714C$category)

#First off: which category should be the baseline?

prop.table(table(ks714C$category,ks714C$outcome),margin=1)

#Art remains the baseline.

#Second off: how do the estimated goal effects look now?

boxplot(log_goal~category,ks714C)

#Let's see those smoothing lines

make_line<-function(subcategory,color){
  subdata<-subset(ks714C,category==subcategory)
  lines(smooth.spline(subdata$norm_goal,subdata$outcome, spar = 1.2), col = color,lwd=2)
}

plot(x=ks714C$norm_goal,y=ks714C$outcome)

linecolors<-c("red","orange","green","blue","purple")

sapply(1:5,function(i)make_line(levels(ks714C$category)[i],linecolors[i]))


#SUBMODEL 1: See how the log_goal now depends on the category

submodel_1<-glm(outcome~norm_goal*category,ks714C,family=binomial())

#See how the log goal effect is across each category

log_goal_vals<-seq(-5,4.5,.1)

predict_df<-data.frame(norm_goal=rep(log_goal_vals,nlevels(ks714C$category)),
                       category=rep(levels(ks714C$category),each=length(log_goal_vals)))

sm1_pred<-predict(submodel_1,predict_df,type="response")

predict_df$pred<-sm1_pred

cat_goal_effects<-do.call(cbind,split(predict_df$pred,predict_df$category))

matplot(x=log_goal_vals,y=cat_goal_effects,type='l',xlab="Normalized Log Goal",ylab="P(Success)",lwd=2)
legend("topright", inset=.05, legend=levels(ks714C$category),col=1:5,lty=1:5,lwd=2)

#SUBMODEL 2: See if the normalized goal effect is quadratic (This model may be dropped later)

submodel_2<-glm(outcome~norm_goal*category+I(norm_goal^2)*category,ks714C,family=binomial())

sm2_pred<-predict(submodel_2,predict_df,type="response")

predict_df$pred2<-sm2_pred

cat_goal_effects<-do.call(cbind,split(predict_df$pred2,predict_df$category))

matplot(x=log_goal_vals,y=cat_goal_effects,type='l',xlab="Normalized Log Goal",ylab="P(Success)",lwd=2)
legend("topright", inset=.05, legend=levels(ks714C$category),col=1:5,lty=1:5,lwd=2)

#TO-DO for next session: Insert auxiliary features
#Conduct NLP analysis
#Consider normalizing the duration variable
#Prove that the auxiliary features 'improve' the accuracy/fit of the model.
#Once this is proven, the project is complete, and then the write-up starts.
#THIS NEEDS TO BE FINISHED BY NEXT WEEKEND, no exceptions! The write-up needs attention and detail.

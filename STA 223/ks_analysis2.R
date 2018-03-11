#Kickstarter Project, Take III

library(glmnet)
library(MASS)
library(data.table)
library(ggplot2)
library(xtable)
library(plyr)
library(pROC)
library(caret)

setwd("../Desktop")

set.seed(223)

ks714<-fread("ks714_v1.csv")

approxAcc<-function(yhat,y,cutoff){
  pred<-(yhat>=cutoff)*1
  confmat<-table(pred,y)
  acc<-sum(diag(confmat))/length(y)
  return(acc)
}

categoryNorm<-function(X,catVar,dataF,dataT,useWith=FALSE){
  #This function normalizes a variable by the tuning set, applies normalization across full data
  mean_SD<-aggregate(as.formula(paste0(X,"~",catVar)),dataT,function(x)c(mean(x),sd(x)))
  mean_SD<-data.frame(mean_SD[,1],mean_SD[,2][,1],mean_SD[,2][,2])
  colnames(mean_SD)<-c(catVar,"mean","sd")
  data_meanSD<-join(dataF,mean_SD)[,c("mean","sd")]
  if(useWith==FALSE){
    data_X<-dataF[,X,with=FALSE]
  }
  else{
    data_X<-dataF[,X]
  }
  norm_X<-(data_X-data_meanSD[,1])/data_meanSD[,2]
  return(norm_X)
}

predictAll<-function(data,folds,model_formula){
  pred_list<-lapply(1:length(folds),function(i)predictOne(data,folds,i,model_formula))
  preds<-unlist(pred_list)
  preds_ord<-preds[order(as.numeric(names(preds)))]
  return(preds_ord)
}

predictOne<-function(data,folds,i,model_formula){
  test_TF<-1:nrow(data)%in%folds[[i]]
  train<-data[test_TF==FALSE,]
  test<-data[test_TF==TRUE,]
  fit<-glm(as.formula(model_formula),train,family=binomial())
  predict<-predict(fit,type="response",test)
  names(predict)<-folds[[i]]
  return(predict)
}

##PART I: Choice of Categories
#We keep the top 5 categories

ks714$category_combo<-factor(paste0(ks714$main_category,":",ks714$category))

top5cats<-names(tail(sort(table(ks714$category_combo)),5))

#Keep the top 5, drop the combo column, subset the data
ks714L<-ks714[ks714$category_combo%in%top5cats,-21]

#Also drop NL/NZ due to their sparse count in this subset.
ks714L<-ks714L[grepl("N[ZL]",ks714L$country)==FALSE,]

#Our remaining categories are: Art, Documentaries, Food, Music and Product Design

#CHOICE OF TEST SET, TRAINING SET AND TUNING SET:
#Split data into four blocks
folds<-createFolds(ks714L$outcome,k=4)

#Initial set-up, Folds 1&2 form training, 3 forms test, 4 forms tuning

train_idx<-c(folds[[1]],folds[[2]])
test_idx<-folds[[3]]
tune_idx<-folds[[4]]

#Part II: Exploratory Data Analysis
#We have four essential predictors: goal, duration, country, category
#We want to examine their effects

##i. CATEGORY##

#Simple question: What is the success rate by category?

barplot(prop.table(table(ks714L$category,ks714L$outcome),margin=1)[,2],ylab="Success Rate",main="Success Rate by Category")

#Art hits right around the baseline...20%. Documentaries and product designs are more likely to be successful.
#Food seems to be causing the drag down in success rate.

##ii. GOAL##

#A log transform is common for large numeric measures. It would work well here for the money as well.

ks714L$log_goal<-log(ks714L$usd_goal_real)

#Make the tuning set now, since we just added log goal

#Let's normalize the log goal, it will help distinguish what will succed and not succeed.

ks714L$norm_goal<-categoryNorm("log_goal","category",ks714L,ks714L)

#What does the norm goal look like across project categories?

p <- ggplot(ks714L, aes(x=category, y=usd_goal_real,fill=factor(outcome,labels=c("Fail","Success")))) + 
  geom_violin(position=position_dodge(1))+scale_y_log10(labels = scales::comma,breaks=c(0,10^seq(0,9)))
p<-p+ylab("Goal (USD, real dollars)")+xlab("")
p<-p+scale_fill_grey()+theme_classic()+labs(fill='Outcome')+ggtitle("How do projects succeed across each category?")
p<-p+theme(plot.title = element_text(hjust = 0.5))
p

#Some interesting patterns here, mainly that food has its successes at much lower amounts.
#Music is more likely to be successful at higher amounts.

##iii. DURATION##

#What does the frequency of duration look like?

duration_counts<-as.vector(table(factor(ks714L$duration,levels=seq(1,60))))

plot(x=seq(1,60),y=duration_counts,type='b',xlab="Duration (days)",ylab="Count",main="Frequency Distribution for Duration")

#We'll also normalize duration within category

ks714L$norm_duration<-categoryNorm("duration","category",ks714L,ks714TU)

#What does the duration look against outcome?

plot(x=ks714L$norm_duration,y=ks714L$outcome,xlab="Duration, normalized by category",ylab="Project Outcome",
     main="Examining the 'Duration Effect'")
lines(smooth.spline(ks714L$norm_duration,ks714L$outcome, spar = 1.1), col = "blue",lwd=2)
abline(h=mean(ks714L$outcome),col="red",lwd=2)

#Duration looks to be a negative effect. The longer the project, the less likely to succeed.

#How does duration look versus the normalized goal?
plot(x=ks714L$norm_duration,y=ks714L$norm_goal,xlab="Normalized Duration",ylab="Normalized Goal",
     main="Examining the Goal-Duration relationship")
lines(smooth.spline(ks714L$norm_duration,ks714L$norm_goal, spar = 1.3), col = "blue",lwd=2)

#Positive linear effect, the longer the duration, the more money asked for

##iv. COUNTRY##

ks714L$country<-factor(ks714L$country,labels=c("Australia","Canada","Great Britain","United States"))

#What is the success rate by country?
barplot(prop.table(table(ks714L$country,ks714L$outcome),margin=1)[,2],ylab="Success Rate",main="Success Rate by Country")

p <- ggplot(ks714L, aes(x=country, y=norm_goal,fill=factor(outcome,labels=c("Fail","Success")))) + 
  geom_violin(position=position_dodge(1))
p<-p+ylab("Goal, normalized by category")+xlab("")
p<-p+scale_fill_grey()+theme_classic()+labs(fill='Outcome')+ggtitle("How well does each country fund projects?")
p<-p+theme(plot.title = element_text(hjust = 0.5))
p

#US is more likely to fund 'extravagant' projects.

#Are there specific category/country combos that are unusually successful?

catcou<-aggregate(outcome~country+category,ks714L,length)
catcou2<-aggregate(outcome~country+category,ks714L,sum)
catcou$rate<-(catcou2$outcome+1)/(catcou$outcome+2)
catcou$base<-join(catcou,setNames(aggregate(outcome~category,ks714L,mean),c("category","catout")),by="category")$catout
catcou$impact<-log((catcou$rate)/(1-catcou$rate))-log(catcou$base/(1-catcou$base))
treemap(catcou,index=c("country","category"),vSize="outcome",vColor="impact",type="value",palette="Greens",
        title="Are certain countries better at funding projects?",title.legend="")

#US has balanced taste. Great Britain prefers product design over food. Canada dislikes music projects.
#Australia is a bit harder to interpret due to size issues, but they prefer art, dislike product design/documentary.

#Part III: Initial model fitting

#Split the data into initial training and test set

ks714TR<-ks714L[train_idx,]
ks714TE<-ks714L[test_idx,]
ks714TU<-ks714L[tune_idx,]

#See the prediction accuracy rate for the simplest model, predict success based on norm_goal

ks714L$m1pred<-predictAll(ks714L,folds,"outcome~norm_goal")

m1_roc<-roc(outcome~m1pred,ks714L)
plot(m1_roc)

m1_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714L$m1pred,ks714L$outcome,x))

#.5955 AUC, not very strong.

ks714L$m2pred<-predictAll(ks714L,folds,"outcome~norm_goal*category")

m2_roc<-roc(outcome~m2pred,ks714L)
plot(m2_roc)

#.6270 AUC, an improvement

m2_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714TE$m2pred,ks714TE$outcome,x))

#Now try adding country

ks714L$m3pred<-predictAll(ks714L,folds,"outcome~norm_goal*(category+country)")

m3_roc<-roc(outcome~m3pred,ks714L)
plot(m3_roc)

#.6467, does help

m3_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714L$m4pred,ks714L$outcome,x))

ks714L$m4pred<-predictAll(ks714L,folds,"outcome~(category+country)*(norm_goal+norm_duration)")

m4_roc<-roc(outcome~m4pred,ks714L)
plot(m4_roc)

m4_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714L$m4pred,ks714L$outcome,x))

#.6586 also helps.

#A bit better at .632. Still have weak effects unfortunately.

m4_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714TE$m4pred,ks714TE$outcome,x))

#Here's a plot of the accuracy across each model for cutoff value

matplot(x=seq(.01,.99,.01),do.call(cbind,list(m1_acc,m2_acc,m3_acc,m4_acc)),type='l')

#Model 4 is clearly the best model for prediction so far. But can we improve it?

#Let's examine some new variables
ks714NL<-read.csv("ks714NLP.csv")

#Also, re drop NL and NZ for now

ks714NL<-ks714NL[grepl("N[ZL]",ks714NL$country)==FALSE,]

ks714NL$country<-factor(ks714NL$country,labels=c("Australia","Canada","Great Britain","United States"))

#What do the 'text scores' look like?

qplot(x=ks714NL$score1,y=ks714NL$score2,color=ks714NL$category)

#The variation in the text data is massively explained by the 'Food' category.

ks714NL$Weird<-(ks714NL$score1>0.1|ks714NL$score2>0.1)*1
ks714NL$category2<-ifelse(ks714NL$Weird==1,"Alternative",as.character(ks714NL$category))

#We now re-define the categories by adding the 'Potato Salad' factor
#Example: look at the log hours until launch with potato salad included
boxplot(log_goal~Weird*outcome*category,ks714NL)

#We renormalize based on category

ks714NL$norm_goal<-categoryNorm("log_goal","category2",ks714NL,ks714NL,useWith=TRUE)
ks714NL$normhours<-categoryNorm("loghours","category2",ks714NL,ks714NL,useWith=TRUE)
ks714NL$norm_duration<-categoryNorm("duration","category2",ks714NL,ks714NL,useWith=TRUE)

#Let's examine a few predictor relationships

#'Alternative projects' were launched the fastest.

#Model 5: Measure norm_goal+normhours vs. category

ks714NL$category2<-relevel(factor(ks714NL$category2),"Art")

ks714NL$m5pred<-predictAll(ks714NL,folds,"outcome~(norm_goal+normhours)*category2")

m5_roc<-roc(outcome~m5pred,ks714NL)

m5_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714NL$m5pred,ks714NL$outcome,x))

#Model 6: Include the 'weird' variable

ks714NL$m6pred<-predictAll(ks714NL,folds,"outcome~(norm_goal+normhours+norm_goal*normhours+norm_duration)*category2")

m6_roc<-roc(outcome~m6pred,ks714NL)

m6_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714NL$m6pred,ks714NL$outcome,x))

#CURRENT TABLE:
#MODEL 1: AUC .5955
#MODEL 2: AUC .6270
#MODEL 3: AUC .6467
#MODEL 4: AUC .6586
#MODEL 5: AUC .7018
#MODEL 6: AUC .7182

#Accuracy plot:
matplot(x=seq(.01,.99,.01),do.call(cbind,list(m1_acc,m2_acc,m3_acc,m4_acc,m5_acc,m6_acc)),type='l')

#Models 1-4: approx 50% accuracy at mean cutoff
#Models 5-6: approx 65% accuracy at mean cutoff

#How has the roc improved?

ROC_list<-list(m1_roc,m2_roc,m3_roc,m4_roc,m5_roc,m6_roc)

sapply(1:length(ROC_list),function(x)lines(ROC_list[[x]],lty=x))

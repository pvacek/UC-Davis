#Kickstarter Project, Take III

library(glmnet)
library(MASS)
library(data.table)
library(ggplot2)
library(xtable)
library(plyr)
library(pROC)
library(caret)
library(grid)
library(xtable)
library(ggalt)

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

ks714L$category<-factor(ks714L$category)

#Our remaining categories are: Art, Documentaries, Food, Music and Product Design

#Split data into five folds for cross validation
folds<-createFolds(ks714L$outcome,k=5)

#Part II: Exploratory Data Analysis
#We have four essential predictors: goal, duration, country, category
#We want to examine their effects

##i. CATEGORY##

#Simple question: What is the success rate by category?

succ_table<-sort(prop.table(table(ks714L$category,ks714L$outcome),margin=1)[,2])
succ_df<-data.frame(Name=names(succ_table),Rate=as.vector(succ_table))
succ_df$Count<-c(870,337,299,290,422)

p<-ggplot(succ_df,aes(x=Name,y=Rate))+geom_bar(stat="identity",width=0.5,fill="gray",color="black")
p<-p+coord_flip()+theme_classic()+ylab("Project Success Rate")+xlab("")
p<-p+geom_text(aes(label=Count), hjust=1.5, color="black", size=5)
p<-p+theme(axis.text=element_text(size=16),
           axis.title=element_text(size=14))
p

ggsave("catsucc.png",plot=last_plot(),width = 7, height = 4.5)

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
p<-p+scale_fill_grey()+theme_classic()+labs(fill='Outcome')
p<-p+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=14),
                                                         axis.title=element_text(size=14))
p

ggsave("catviol.png",plot=last_plot(),width = 7, height = 4.5)

#Some interesting patterns here, mainly that food has its successes at much lower amounts.
#Music is more likely to be successful at higher amounts.

#What does the goal effect look like across categories?

categoryEffect<-function(cat,data,h){
  subdata<-subset(data,category==cat)
  smoother<-smooth.spline(subdata$norm_goal,subdata$outcome, spar = h)
  return(smoother)
}

categories<-levels(ks714L$category)

smoothers<-sapply(1:nlevels(ks714L$category),function(i)categoryEffect(categories[i],ks714L,1.2))

p<-ggplot(ks714L,aes(x=norm_goal,y=outcome))+geom_point(alpha=.25)
p<-p+geom_smooth(method="glm",method.args=list(family="binomial"),aes(linetype=category),
                 fullrange=TRUE,color="black",se=FALSE)
p<-p+theme_classic()+xlab("Log Goal, USD, normalized by category")+ylab("")
p<-p+theme(plot.title = element_text(hjust = 0.5))
p<-p+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
p<-p+theme(legend.position="none")+theme(axis.text=element_text(size=14),
                                         axis.title=element_text(size=14))
p
grid.text("Music",x=unit(0.04, "npc"), y = unit(0.24, "npc"))
grid.text("Design",x=unit(0.04, "npc"), y = unit(0.70, "npc"))
grid.text("Docu.",x=unit(0.04, "npc"), y = unit(0.67,"npc"))
grid.text("Art",x=unit(0.04, "npc"), y = unit(0.80,"npc"))
grid.text("Food",x=unit(0.04, "npc"), y = unit(0.77,"npc"))

ggsave("catlogit.png",plot=last_plot(),width = 7, height = 4.5)

##iii. DURATION##

#What does the frequency of duration look like?

duration_counts<-as.vector(table(factor(ks714L$duration,levels=seq(1,60))))

p<-ggplot(data=data.frame(duration_counts),aes(x=seq(1,60),y=duration_counts))+geom_point()+geom_line()
p<-p+theme_classic()+xlab("Duration (days)")+ylab("Count")
p

ggsave("durationcount.png",plot=last_plot(),width = 7, height = 4.5)

#We'll also normalize duration within category

ks714L$norm_duration<-categoryNorm("duration","category",ks714L,ks714L)

#What does the duration look against outcome?

p<-ggplot(ks714L,aes(x=norm_duration,y=outcome))+geom_point(alpha=.25)
p<-p+geom_smooth(method="glm",method.args=list(family="binomial"),aes(linetype=category),
                 fullrange=TRUE,color="black",se=FALSE)
p<-p+theme_classic()+xlab("Duration (days), normalized by category")+ylab("")
p<-p+theme(plot.title = element_text(hjust = 0.5))
p<-p+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
p<-p+theme(legend.position="none")+theme(axis.text=element_text(size=14),
                                         axis.title=element_text(size=14))
p

ggsave("normduration.png",plot=last_plot(),width = 7, height = 4.5)

#Duration looks to be a negative effect. The longer the project, the less likely to succeed.

#How does duration look versus the normalized goal?

p<-ggplot(ks714L,aes(x=norm_duration,y=norm_goal))+geom_point(alpha=.25)
p<-p+geom_smooth(method="lm",fullrange=TRUE,color="black",se=FALSE)
p<-p+theme_classic()+xlab("Duration (days), normalized by category")+ylab("Log Goal, USD, normalized by category")
p<-p+theme(plot.title = element_text(hjust = 0.5))
p<-p+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.line.y=element_blank())
p<-p+theme(legend.position="none")+theme(axis.text=element_text(size=14),
                                         axis.title=element_text(size=14))
p

ggsave("durgoal.png",plot=last_plot(),width = 7, height = 4.5)

#Positive linear effect, the longer the duration, the more money asked for

##iv. COUNTRY##

ks714L$country<-factor(ks714L$country,labels=c("Australia","Canada","Great Britain","United States"))

#What is the success rate by country?

cou_table<-sort(prop.table(table(ks714L$country,ks714L$outcome),margin=1)[,2])
cou_df<-data.frame(Name=names(cou_table),Rate=as.vector(cou_table))
cou_df$Count<-c(68,168,242,1740)

p<-ggplot(cou_df,aes(x=Name,y=Rate))+geom_bar(stat="identity",width=0.5,fill="gray",color="black")
p<-p+coord_flip()+theme_classic()+ylab("Project Success Rate")+xlab("")
p<-p+geom_text(aes(label=Count), hjust=1.5, color="black", size=5)
p<-p+theme(axis.text=element_text(size=16),
           axis.title=element_text(size=14))
p

ggsave("countrysucc.png",plot=last_plot(),width = 7, height = 4.5)

p <- ggplot(ks714L, aes(x=country, y=norm_goal,fill=factor(outcome,labels=c("Fail","Success")))) + 
  geom_violin(position=position_dodge(1))
p<-p+ylab("Goal, normalized by category")+xlab("")
p<-p+scale_fill_grey()+theme_classic()+labs(fill='Outcome')+ggtitle("How well does each country fund projects?")
p<-p+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=14),
                                                         axis.title=element_text(size=14))
p

ggsave("countryfund.png",plot=last_plot(),width = 7, height = 4.5)

#US is more likely to fund 'extravagant' projects.

#Part III: Initial model fitting

#See the prediction accuracy rate for the simplest model, predict success based on norm_goal

ks714L$m1pred<-predictAll(ks714L,folds,"outcome~norm_goal")

m1_roc<-roc(outcome~m1pred,ks714L)

m1_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714L$m1pred,ks714L$outcome,x))

#.5955 AUC, not very strong.

ks714L$m2pred<-predictAll(ks714L,folds,"outcome~norm_goal*category")

m2_roc<-roc(outcome~m2pred,ks714L)
plot(m2_roc)

#.6270 AUC, an improvement

m2_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714L$m2pred,ks714L$outcome,x))

#Now try adding country

ks714L$m3pred<-predictAll(ks714L,folds,"outcome~norm_goal*(category+country)")

m3_roc<-roc(outcome~m3pred,ks714L)
plot(m3_roc)

#.6467, does help

m3_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714L$m3pred,ks714L$outcome,x))

ks714L$m4pred<-predictAll(ks714L,folds,"outcome~(category+country)*(norm_goal+norm_duration)")

m4_roc<-roc(outcome~m4pred,ks714L)
plot(m4_roc)

m4_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714L$m4pred,ks714L$outcome,x))

#.6586 also helps.

#A bit better at .632. Still have weak effects unfortunately.

m4_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714L$m4pred,ks714L$outcome,x))

#Model 4 is clearly the best model for prediction so far. But can we improve it?

#Let's examine some new variables
ks714NL<-read.csv("ks714NLP.csv")

#Also, re drop NL and NZ for now

ks714NL<-ks714NL[grepl("N[ZL]",ks714NL$country)==FALSE,]

ks714NL$country<-factor(ks714NL$country,labels=c("Australia","Canada","Great Britain","United States"))

#What do the 'text scores' look like?

p<-ggplot(data=ks714NL,aes(x=score1,y=score2))+geom_point(aes(shape=category),alpha=.5,size=2)+scale_shape_manual(values=c(15,16,17,18,19))+theme_classic()
p<-p+xlab("Text PCA Score 1")+ylab("Text PCA Score 2")+xlim(NA,0.6)+ylim(NA,0.6)
p<-p+geom_encircle(data=subset(ks714NL,score1>.1),s_shape=0.9)+geom_encircle(data=subset(ks714NL,score2>.1),s_shape=0.9)
p<-p+geom_text(x=0.285,y=0.2,label="Group 1:\n Score1>.1",size=5)+geom_text(x=.13,y=0.25,label="Group 2:\n Score2>.1",size=5)
p<-p+theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14))
p

ggsave("textpca.png",plot=last_plot(),width = 7, height = 4.5)

#The variation in the text data is massively explained by the 'Food' category.

ks714NL$Weird<-(ks714NL$score1>0.1|ks714NL$score2>0.1)*1
ks714NL$category2<-factor(ifelse(ks714NL$Weird==1,"Alternative",as.character(ks714NL$category)))

#We renormalize based on category

ks714NL$norm_goal<-categoryNorm("log_goal","category2",ks714NL,ks714NL,useWith=TRUE)
ks714NL$normhours<-categoryNorm("loghours","category2",ks714NL,ks714NL,useWith=TRUE)
ks714NL$norm_duration<-categoryNorm("duration","category2",ks714NL,ks714NL,useWith=TRUE)

#Let's examine a few predictor relationships

categories<-levels(ks714NL$category2)
NY_labels<-rep(c("Fail","Succ."),6)

p <- ggplot(ks714NL, aes(x=category2, y=usd_goal_real,fill=factor(outcome,labels=c("Fail","Success")))) + 
  geom_boxplot(position=position_dodge(1))+scale_y_log10(labels = scales::comma,breaks=c(0,10^seq(0,9)))
p<-p+ylab("Goal (USD, real dollars)")+xlab("")
p<-p+scale_fill_grey()+theme_classic()+labs(fill='Outcome')
p<-p+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=14),
                                                         axis.title=element_text(size=14))
p<-p+scale_fill_manual(values=c("#636363","#f0f0f0"))
p

ggsave("goalcat2.png",plot=last_plot(),width = 7, height = 4.5)

p <- ggplot(ks714NL, aes(x=factor(Weird,labels=c("Regular","Weird")),
                         y=usd_goal_real,fill=factor(outcome,labels=c("Fail","Success")))) + 
  geom_boxplot(position=position_dodge(1))+scale_y_log10(labels = scales::comma,breaks=c(0,10^seq(0,9)))
p<-p+ylab("Goal (USD, real dollars)")+xlab("")
p<-p+scale_fill_grey()+theme_classic()+labs(fill='Outcome')
p<-p+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=14),
                                                         axis.title=element_text(size=14))
p<-p+scale_fill_manual(values=c("#636363","#f0f0f0"))
p

ggsave("goalweird.png",plot=last_plot(),width = 7, height = 4.5)

p <- ggplot(ks714NL, aes(x=category2, y=loghours,fill=factor(outcome,labels=c("Fail","Success")))) + 
  geom_boxplot(position=position_dodge(1))
p<-p+ylab("Log Hours, non-normalized")+xlab("")
p<-p+scale_fill_grey()+theme_classic()+labs(fill='Outcome')
p<-p+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=14),
                                                         axis.title=element_text(size=14))
p<-p+scale_fill_manual(values=c("#636363","#f0f0f0"))
p

ggsave("hourcat2.png",plot=last_plot(),width = 7, height = 4.5)

p <- ggplot(ks714NL, aes(x=factor(Weird,labels=c("Regular","Weird")),
                         y=loghours,fill=factor(outcome,labels=c("Fail","Success")))) + 
  geom_boxplot(position=position_dodge(1))
p<-p+ylab("Log Hours, non-normalized")+xlab("")
p<-p+scale_fill_grey()+theme_classic()+labs(fill='Outcome')
p<-p+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=14),
                                                         axis.title=element_text(size=14))
p<-p+scale_fill_manual(values=c("#636363","#f0f0f0"))
p

ggsave("hourweird.png",plot=last_plot(),width = 7, height = 4.5)

#'Alternative projects' were launched the fastest.

#Model 5: Measure norm_goal+normhours vs. category

ks714NL$category2<-relevel(factor(ks714NL$category2),"Art")

ks714NL$m5pred<-predictAll(ks714NL,folds,"outcome~(norm_goal+normhours)*category2")

m5_roc<-roc(outcome~m5pred,ks714NL)

m5_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714NL$m5pred,ks714NL$outcome,x))

#Model 6: Include extra terms

ks714NL$m6pred<-predictAll(ks714NL,folds,"outcome~(norm_goal+normhours+norm_goal*normhours+norm_duration)*category2")

m6_roc<-roc(outcome~m6pred,ks714NL)

m6_acc<-sapply(seq(.01,.99,.01),function(x)approxAcc(ks714NL$m6pred,ks714NL$outcome,x))

#CURRENT TABLE:
#MODEL 1: AUC .6043, Acc.
#MODEL 2: AUC .6572, Acc.
#MODEL 3: AUC .6472, Acc.
#MODEL 4: AUC .6610, Acc.
#MODEL 5: AUC .7028, Acc.
#MODEL 6: AUC .7213, Acc.

acc_list<-list(m1_acc,m2_acc,m3_acc,m4_acc,m5_acc,m6_acc)

matplot(x=seq(.01,.99,.01),do.call(cbind,acc_list),xlim=c(.05,.25),xlab="Cutoff Value",ylab="Accuracy Rate",
        main="Comparing model accuracies")
abline(v=.2,lty=2)

ROC_list<-list(m1_roc,m2_roc,m3_roc,m4_roc,m5_roc,m6_roc)

png("roc.png",width=1100,height=1100)
plot(x=m1_roc$specificities,y=m1_roc$sensitivities,type='l',xlim=c(1,0),lty=2,lwd=1,cex=2,xlab="",ylab="",cex.axis=1.5)
title(xlab="Specificity",ylab="Sensitivity", line=2.5, cex.lab=1.5)
lines(x=seq(1,0,-.01),y=seq(0,1,.01),lwd=2,col="red")
sapply(2:4,function(i)lines(x=ROC_list[[i]]$specificities,y=ROC_list[[i]]$sensitivities,lty=2,lwd=1))
sapply(5:6,function(i)lines(x=ROC_list[[i]]$specificities,y=ROC_list[[i]]$sensitivities,lwd=2))
dev.off()

#Which projects were hardest to predict?
predmat<-cbind(ks714L$m1pred,ks714L$m2pred,ks714L$m3pred,ks714L$m4pred,ks714NL$m5pred,ks714NL$m6pred)
outcomemat<-predmat>=mean(ks714L$outcome)
num_correct<-apply(ks714L$outcome==outcomemat,1,sum)

#How many projects were guessed incorrectly across ALL models?
xtable(table(num_correct))

#Did these models do better or worse on specific categories?
xtable(aggregate(num_correct~category2,ks714NL,mean))

#It seems to be that Food category was easiest to predict, while the Product Design category was the hardest.

#360 projects did not agree with any of our models.

#Averaging out our model probabilities gives us a 'naive' estimate of what we think should be a successful project.
avg_probs<-apply(predmat,1,mean)

#We can look at some projects that disagree with these averages to get an idea of what we should have investigated
#Which project had the highest average probability but failed?

xtable(ks714NL[which.max(avg_probs),c("name","usd_goal_real")])

#The model seems to point in the right direction here, it's just that the project was canceled.
#This suggests that cancelled projects may be an issue with regards to prediction.

#What project had the lowest average probability but succeeded?
xtable(subset(ks714NL,outcome==1)[which.min(avg_probs[ks714NL$outcome==1]),c("name","usd_goal_real")])

#Our models were very harsh on projects from the food category. This project also seemed to ask for a lot of money.
#This project seems much more serious than the other food ones. So using sentiment analysis would have helped here.

#MAKING TABLES:

#Model fits

model_tbl1<-data.frame(Model=c("Model 1","Model 2","Model 3","Model 4"),
           Predictors=c("~norm_goal","~goal*category","~goal*(category+country)","~(goal+duration)*(category+country)"),
           AUC=c(.6043,.6527,.6472,.6610),Acc=c(.5564,.5406,.5423,.5739))

xtable(model_tbl1,digits=4)

model_tbl2<-data.frame(Model=c("Model 5","Model 6"),Predictors=c("~(goal+hours)*category2","~(goal+hours+goal*hours+duration)*category2"),
           AUC=c(.7028,.7213),Acc=c(.6366,.6344))

xtable(model_tbl2,digits=4)

#Text tables

text_table<-data.frame(Group=c("Score1<=.1&Score2<=.1","Score1>.1","Score2>.1"),first=c("make","salad","make"),
                       second=c("help","potato","pizza"),third=c("want","make","want"),
                       fourth=c("need","want","cookie"),fifth=c("music","better","chip"))

xtable(text_table)



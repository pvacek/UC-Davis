library(glmnet)
library(MASS)
library(data.table)
library(ggplot2)
library(xtable)

setwd("../Desktop")

#Precision and recall functions

prec_recall<-function(cutoff,predicted,actual){
  decision<-as.numeric(predicted>=cutoff)
  conf_mat<-table(decision,actual)
  if(nrow(conf_mat)>1){
  precision<-conf_mat[2,2]/sum(conf_mat[2,])
  recall<-conf_mat[2,2]/sum(conf_mat[,2])
  return(c(precision,recall))}
  else{
    return(c(0,0))
  }
}

#Sparse category analysis of July 2014 topics

ks714<-fread("ks714_v1.csv")

ks714$category<-factor(ks714$category)

ks714$main_category<-factor(ks714$main_category)


#Make the categories into a data matrix

cat_data_matrix<-sparse.model.matrix(outcome~main_category/category,data=ks714)[,-1]

cv.fit<-glmnet(cat_data_matrix,ks714$outcome,alpha=1,family="binomial")

cv.out<-cv.glmnet(cat_data_matrix,ks714$outcome,alpha=1,family="binomial",type.measure="mse")

plot(cv.out$glmnet.fit,"lambda")

selected_coefs<-coef(cv.out,s="lambda.1se")

#Find which categories to remove

remaining_coefs<-selected_coefs[which(selected_coefs!=0),1]

keep_cats<-names(remaining_coefs)[grep(":.+",names(remaining_coefs))]
keep_cats<-gsub("^.+:category","",keep_cats)

keep_mains<-remaining_coefs[grepl(":.+",names(remaining_coefs))==FALSE][-1]
keep_mains<-gsub("main_category","",names(keep_mains))


#Make a copy, have this one with reduced categories

ks714_v2<-ks714

ks714_v2$category<-factor(ifelse(ks714_v2$category%in%keep_cats==FALSE,"Other",as.character(ks714_v2$category)))
ks714_v2$main_category<-factor(ifelse(ks714_v2$main_category%in%keep_mains==FALSE,"Misc",as.character(ks714_v2$main_category)))

table(ks714_v2$category,ks714_v2$outcome)
table(ks714_v2$main_category,ks714_v2$outcome)

write.csv(ks714_v2,"ks714_v2.csv",row.names=FALSE)

###MODEL 1: Using log goal to predict success

ks714_v2$log_goal<-log(ks714$usd_goal_real)

model_1<-glm(outcome~log_goal,ks714_v2,family=binomial())

model_1pr<-predict(model_1,data.frame(log_goal=sort(ks714_v2$log_goal)))
model_1pihat<-exp(model_1pr)/(1+exp(model_1pr))

model_1_metrics<-sapply(seq(.06,.43,.01),function(x)prec_recall(x,model_1pihat,ks714_v2$outcome))

matplot(x=seq(.06,.43,.01),t(model_1_metrics),type='l',xlab="cutoff",ylab="score")

###MODEL 2: Using log goal + categories to predict success

#Initial model fit: outcome~main_cat+category+country+duration+goal
#Set misc/other as baseline classes

ks714_v2$category<-relevel(ks714_v2$category,ref="Other")
ks714_v2$main_category<-relevel(ks714_v2$main_category,ref="Misc")
ks714_v2$country<-factor(ks714_v2$country)
ks714_v2$country<-relevel(ks714_v2$country,ref="US")

model_2<-glm(outcome~log_goal+main_category+category+country,ks714_v2,family=binomial())

#First attempt at prediction, look at classification metrics

model_2pr<-predict(model_2)

model_2pihat<-exp(model_2pr)/(1+exp(model_2pr))

model_2_metrics<-sapply(seq(.01,.8,.01),function(x)prec_recall(x,model_2pihat,ks714_v2$outcome))

matplot(x=seq(.01,.8,.01),t(model_2_metrics),type='l',xlab="cutoff",ylab="score")

#Next attempt, start fixing goal and duration, there must be patterns we're missing here

ks714_v2$cat_goal<-cut(ks714_v2$log_goal,c(seq(-.1,12,1),20))
ks714_v2$cat_dur<-cut(ks714_v2$duration,c(0,10,20,29,30,40,59,60))

gd_data_matrix<-sparse.model.matrix(~cat_dur+cat_goal+cat_dur*cat_goal,data=ks714_v2)

cv_gd.out<-cv.glmnet(gd_data_matrix,ks714$outcome,alpha=1,family="binomial",type.measure="mse")

selected_coefs<-coef(cv_gd.out,s="lambda.1se")

ks714_v2$cat_dur2<-cut(ks714_v2$duration,c(0,10,20,29,30,40,60))
ks714_v2$cat_dur2<-relevel(ks714_v2$cat_dur2,ref="(29,30]")
ks714_v2$cat_goal2<-as.integer(cut(ks714_v2$log_goal,c(-.1,1,2,3,10,11,12,20),1:7))

#Quick: examine relationship between duration and goal

dur_goal_tab<-table(ks714_v2$cat_dur2,ks714_v2$cat_goal2,ks714_v2$outcome)
succ_rate_tab<-dur_goal_tab[,,2]/(dur_goal_tab[,,1]+dur_goal_tab[,,2])
succ_rate_tab[5,1]<-0
succ_rate_df<-melt(succ_rate_tab)

hm<-ggplot(succ_rate_df,aes(Var1,Var2))+geom_tile(aes(fill=value),color="white")+scale_fill_gradient(low = "white", high = "steelblue")
hm

#MODEL 3: outcome~goal_ord+duration_category

model_3<-glm(outcome~cat_goal2+cat_dur2,ks714_v2,family=binomial())

model_3pr<-predict(model_3)
model_3pihat<-exp(model_3pr)/(1+exp(model_3pr))

model_3_metrics<-sapply(seq(.01,.8,.01),function(x)prec_recall(x,model_3pihat,ks714_v2$outcome))

matplot(x=seq(.01,.8,.01),t(model_3_metrics),type='l',xlab="cutoff",ylab="score")

#MODEL 4: First-order model. outcome~goal_ord+duration_category+main_category+category+country

model_4<-glm(outcome~cat_goal2+cat_dur2+main_category+category+country,ks714_v2,family=binomial())

model_4pr<-predict(model_4)
model_4pihat<-exp(model_4pr)/(1+exp(model_4pr))

model_4_metrics<-sapply(seq(.01,.8,.01),function(x)prec_recall(x,model_4pihat,ks714_v2$outcome))

matplot(x=seq(.01,.8,.01),t(model_4_metrics),type='l',xlab="cutoff",ylab="score")

model_4_f1<-2*(apply(model_4_metrics,2,prod)/apply(model_4_metrics,2,sum))

plot(x=seq(.01,.8,.01),y=model_4_f1,xlab="cutoff",ylab="F1 score",main="Evaluating F1-Score of First Order Model")

#Recall: 18%, the model is improving
#Precision: 63.9%, not bad

#MODEL 5: Second-Order model selected by Lasso (THE BLACK BOX MODEL, avoid using)

Scope=list(upper=~cat_goal2+cat_dur2+main_category+category+country+CAT,lower=~1)

model_0<-glm(outcome~1,ks714_v2,family=binomial())

m5_data_matrix<-sparse.model.matrix(outcome~(cat_goal2+cat_dur2+main_category/category+country)^2,data=ks714_v2)[,-1]

cv_m5<-cv.glmnet(m5_data_matrix,ks714$outcome,alpha=0.5,family="binomial",type.measure="mse")

selected_coefs<-coef(cv_m5,s="lambda.1se")

model_5pr<-predict(cv_m5,m5_data_matrix,s="lambda.1se")

model_5pihat<-exp(model_5pr)/(1+exp(model_5pr))

model_5_metrics<-sapply(seq(.01,.8,.01),function(x)prec_recall(x,model_5pihat,ks714_v2$outcome))

matplot(x=seq(.01,.8,.01),t(model_5_metrics),type='l',xlab="cutoff",ylab="score")

model_5_f1<-2*(apply(model_5_metrics,2,prod)/apply(model_5_metrics,2,sum))

plot(x=seq(.01,.8,.01),y=model_5_f1,xlab="cutoff",ylab="F1 score",main="Evaluating F1-Score of Second Order Model")

###INTERMEDIATE STEP, make table with precision, recall, F1 at class proportion cutoff value

cutoff_val<-mean(ks714_v2$outcome)

model_preds<-cbind(model_1pihat,model_2pihat,model_3pihat,model_4pihat,model_5pihat)

agg_model_metrics<-t(sapply(1:ncol(model_preds),function(x)prec_recall(cutoff_val,model_preds[,x],ks714_v2$outcome)))

model_f1s<-2*(apply(agg_model_metrics,1,prod)/apply(agg_model_metrics,1,sum))

model_table<-data.frame(model=c("Model 1","Model 2","Model 3","Model 4","Model 5"),agg_model_metrics,F1_score=model_f1s)
colnames(model_table)[2:3]<-c("Precision","Recall")
model_table
xtable(model_table)

#While we're at it...save v3 and the model table

write.csv(ks714_v2,"ks714_v3.csv",row.names=FALSE)

write.csv(model_table,"models.csv",row.names=FALSE)

###
#MODEL 7+: Bring in auxiliary features
###

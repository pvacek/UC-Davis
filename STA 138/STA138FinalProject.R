#STA 138 Final Project

#Read in data

final<-read.table("final.txt",header=TRUE)

#Examine the data

library(ggplot2)
library(GGally)

#Code used from: https://www.r-bloggers.com/scatter-plot-matrices-in-r/

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

#Are any explanatory variable significantly correlated? (Multicollinearity)

pairs(final[,-c(1,5)], upper.panel = panel.cor,col=ifelse(final[,5]==1,"blue","red"))

#Notice beck and mcs have significant correlation

#LOESS fits
#Make a LOESS function

makeLoess<-function(var){
  var.domain=sort(unique(round(final[,var])))
  count<-as.matrix(prop.table(table(round(final[,var]),final[,1]),margin=1))
  lo<-loess(count[,2]~var.domain,degree=2)
  plot(x=var.domain,y=count[,2],xlab=names(final)[var],ylab="Proportion Diagnosed",ylim=c(0,1))
  lines(x=var.domain,y=predict(lo),col="red")
}

#Make 5 plots for each continuous variable
par(mfcol=c(1,5))
sapply(c(2:7)[-4],makeLoess)


#Build logistic regression models
#Null model
null.model<-glm(dav~1,family="binomial",data=final)
summary(null.model)
#Full model
full.model<-glm(dav~pcs+mcs+beck+pgend+age+educat,family="binomial",data=final)
summary(full.model)
#Attemp stepwise model selection
model.select<-step(null.model,scope=list(upper=full.model),direction="both",test="Chisq",data=final)
summary(model.select)
#Check goodness of fit
library(ResourceSelection)
hoslem.test(final$dav,model.select$fitted.values)
#This model produces an *okay* fit, let's try BIC selection too
require(BMA)
model.bic<-bic.glm(dav~.,data=final,glm.family="binomial")
summary(model.bic)

#Select model 2, model 1 has highest posterior probability, but excludes education, which is an influential variable
#Therefore we build our logistic model on the effects of MCS and Education
#Recreate model 2
the.model<-glm(dav~mcs+educat,family="binomial",data=final)
summary(the.model)
#Hypothesis testing: test significance of model
#Use L0-LM/L0
(351.74-304.59)/351.74
#We can observe wald tests of parameters for each variable, MCS very large, educat somewhat large
#Ask for some more hypothesis tests in OH, also: estimation

#Odds ratios
print(paste0("95% Odds Ratio C.I. for MCS: (",round(exp(-.07186-1.96*.01167),4),",",round(exp(-.07186+1.96*.01167),4),")"))
print(paste0("95% Odds Ratio C.I. for education: (",round(exp(.13380-1.96*.05797),4),",",round(exp(.13380+1.96*.05797),4),")"))
#Interpretation: For each 1 unit increase in mental component status of patient, odds of depression decrease by ~4.7 to 8.1%
#Interpretation: For each extra year of education, odds of depression increase from ~2 to 28%

#Show model fit between two variables:

logit.predict<-function(mcs,educat){
  prob<-exp(the.model$coefficients%*%c(1,mcs,educat))/(1+exp(the.model$coefficients%*%c(1,mcs,educat)))
  return(prob)
}

diagnosis.probs<-matrix(mapply(function(x,y)logit.predict(x,y),rep(8:67,each=18),rep(0:17,60)),18,60)

plot(x=8:67,y=diagnosis.probs[1,],ylim=c(0,.8),type='l',xlab="MCS",ylab="P<hat>(Diagnosis)")

sapply(2:17,function(x)lines(x=8:67,y=diagnosis.probs[x,]))
text(x=15,y=0.715,"Education=17")
text(x=15,y=0.135,"Education=0")

#Residual analysis
#Plot residuals
plot(the.model$residuals,ylab="Logit Model Residuals",main="Visual examination of logit residuals")

#Plot residuals vs. fitted values
plot(the.model$fitted.values,the.model$residuals,ylab="Logit Residuals",xlab="Fitted Values",
     main="Fitted values vs. Pearson Residuals")
abline(h=0)

#Plot data values vs. residuals
par(mfrow=c(1,2))
plot(final$mcs,the.model$residuals,xlab="MCS",ylab="Logit Residuals",
     main="MCS vs. Pearson Residuals")
abline(h=0)

plot(final$educ,the.model$residuals,xlab="Years of Education",ylab="Logit Residuals",
     main="Years of Education vs. Pearson Residuals")
abline(h=0)

#Goodness of fit
hoslem.test(final$dav,the.model$fitted.values)
#Very high goodness of fit, but residuals are of concern

##STA 138 Project II##

###I. Descring and Reporting the Data###

#We will be using "saccousins.csv"
#It has two variables

saccousins<-read.csv("saccousins.csv")

#Description of the data
#Y~"win" whether the Kings won or not
#X~"plusminus" the difference in points scored when DeMarcus Cousins was on the court

#Explain how the plus/minus differential works (ask me if necessary)

#Statistical summaries:
table(saccousins$win) #29 wins, 36 losses, not too imbalanced
summary(plusminus) #Tends to be in the negative, range is fairly spread

#A visual interpretation
#+/- vs. win
plot(saccousins$plusminus,saccousins$win,xlab="Plus/minus",ylab="Kings win? (1=Yes)",pch=20,
     main="Visual analysis of DeMarcus Cousins +/- and Kings victories")

#alternative plot, points colored by count
library(ggplot2)

qplot(saccousins$plusminus,saccousins$win,alpha=.1,size=2,col="red",
      main="Visual analysis of DeMarcus Cousins +/- and Kings victories",xlab="Plus/minus",
      ylab="Kings win? (1=Yes)")

###For step 2, this is all write-up###

###III. Estimating model parameters and explainining meanings###

#Fit a logistic model to the data

sac.glm<-glm(win~plusminus,data=saccousins,family="binomial")
summary(sac.glm)

#Plot the fit of the data

plot(x=saccousins$plusminus,y=sac.glm$fitted.values,xlab="Plus/minus",ylab="P(Kings win)",pch=20,
     main="Logistic curve for Cousins +/- effect on Kings victories")

#Interpret parameters

#What does a 1 unit increase look like?
#This change is clearly non-constant
#So let's find the maximum increase
#Use -b0/b1 formula
#Plus minus is discrete, so we will round the value

max.slope<-(-sac.glm$coefficients[1]/sac.glm$coefficients[2])
print(round(max.slope))

#Therefore slope is maximized at 2

#Interpretation: When DeMarcus Cousins has a +/- of 2 the slope is maximized.
#A one unit increase in +/- from 2 to 3 increases the probability of a Kings victory by:
sac.glm$coefficients[2]/4
#Roughly five percent.

###IV. Providing a confidence interval for the Odds Ratio###

#95% confidence interval for odds ratio
#Use $coefficients on the summary of the GLM to get estimate and SE
sac.glm.coef<-summary(sac.glm)$coefficients

sac.OR.CI<-c(exp(sac.glm.coef[2,1]-1.96*sac.glm.coef[2,2]),exp(sac.glm.coef[2,1]+1.96*sac.glm.coef[2,2]))
print(sac.OR.CI)

#Interpretation: 95% confident that a 1 unit increase in DeMarcus Cousins' +/-...
#...increases the probability of the Kings winning the game by 12.66% to 37.43%.

###V. Doing residual analysis###

#Save the residuals in a short variable name
sac.resid<-sac.glm$residuals

#Plot the residuals

plot(sac.resid,ylab="Logit Model Residuals",main="Visual examination of logit residuals")

#We can notice a couple of influential observations

#Plot residuals vs. fitted values
plot(sac.glm$fitted.values,sac.resid,ylab="Logit Residuals",xlab="Fitted Values",
     main="Fitted values vs. Pearson Residuals")
abline(h=0)

#Plot data values vs. residuals
plot(saccousins$plusminus,sac.resid,xlab="Plus/minus",ylab="Logit Residuals",
     main="+/- vs. Pearson Residuals")
abline(h=0)

#Conduct a Hoslem Test to see how well model fits data
library(ResourceSelection)
hoslem.test(saccousins$win,sac.glm$fitted.values)

#Very high p-value, very good fit

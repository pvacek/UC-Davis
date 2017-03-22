#STA 138 take home midterm R code

#Question 9.5
spending<-read.table("spending.txt",header=TRUE)

#Part a.

1-pchisq(31.6695,48)

#Part b.
#i.
exp(2.142)
#ii.
exp(.3183)
#iii.
exp(.7294)

#Part c.
print(paste0("95% Wald C.I.: ",round(exp(2.142-1.96*.5566),4),",",round(exp(2.142+1.96*.5566),4)))

#Question 9.7

seatbelt<-data.frame(expand.grid(S=factor(c("Seat belt","None")),E=factor(c("Yes","No")),I=factor(c("Nonfatal","Fatal"))),count=c(1105,4624,411111,157342,14,497,483,1008))

#Part a.
seatbelt.glm<-glm(count~S+E+I+S*E+S*I+E*I,family="poisson",data=seatbelt)
summary(seatbelt.glm)
1-pchisq(2.85,1)

#Part b.
exp(2.79779)
exp(1.71732)

#Part c.
#Dissimilarity index
diss.index<-sum(abs(seatbelt$count-seatbelt.glm$fitted.values))*(1/(2*sum(seatbelt$count)))
diss.index

#Question 5.3

#Part a.

rondo<-read.table("rondo.txt",header=TRUE)
rondo.glm<-glm(result~assists,family="binomial",data=rondo)
summary(rondo.glm)

#Part b.

#Use a plot to demonstrate probability increases

rondo.probs<-sapply(3:24,function(x)exp(rondo.glm$coef%*%c(1,x))/(1+exp(rondo.glm$coef%*%c(1,x))))

plot(x=3:24,y=rondo.probs,main="Logistic CDF for probability of Celtics win",
     xlab="# of Rondo assists",ylab="P<hat>(Y=1)",ylim=c(0,1),type='b',pch=20)

#Part c.

#Use a Wald test, b1/se(b1)~Z(0,1)
#We already know the values from the summary of the GLM
#So the test is done on the report
#The confidence interval is:

rondo.wald.ci<-c(exp(rondo.glm$coef[2]-1.96*.08325),exp(rondo.glm$coef[2]+1.96*.08325))
names(rondo.wald.ci)<-c("Lower","Upper")
print(rondo.wald.ci)

#95% confident that an extra assist by Rajon Rondo increases odds of Celtics win by 13.9% to 57.9%

#Question 5.6

challenger<-read.table("challenger.txt",header=TRUE)
challenger.glm<-glm(td~temp,family="binomial",data=challenger)

#Part a.

plot(x=challenger$temp,y=challenger.glm$fitted.values,pch=20,main="Logistic CDF for probability of T.D.",ylab="P<hat>(TD)",xlab="Temperature")

#Part b.
#Use exp(b0+b1*31)/(1+exp(b0+b1*31))
exp(challenger.glm$coefficients%*%c(1,31))/(1+exp(challenger.glm$coefficients%*%c(1,31)))

#Part c.
#Statistical significance given by model summary
summary(challenger.glm)
#Wald C.I.
challenger.wald.ci<-c(exp(challenger.glm$coef[2]-1.96*.1082),exp(challenger.glm$coef[2]+1.96*.1082))
challenger.wald.ci
#Interpret as: a 1 unit increase in temp decreases odds of T.D. by 2% to 36%

#Question 5.8
kyphosis<-read.table("kyphosis.txt",header=TRUE)
kyphosis.glm<-glm(present~age,family="binomial",data=kyphosis)
#Part a.
summary(kyphosis.glm)
#Use summary to perform Wald test
#Part b.
plot(y=kyphosis$age,x=kyphosis$present,xlab="Kyphosis present (0=N/1=Y)",ylab="Age (mos.)",main="Dispersion between Kyphosis groups")
kyphosis$agesq<-kyphosis$age^2
kyphosis.glm2<-glm(present~age+agesq,family="binomial",data=kyphosis)
summary(kyphosis.glm2)
#Squared age effect is significant
plot(x=kyphosis$age,y=kyphosis.glm2$fitted.values,ylim=c(0,1),pch=20,xlab="Age (mos.)",
     ylab="P<hat>(Kyphosis)",main="Parabolic Probability of Kyphosis")

###################
#STA 138 Project I#
###################

#Let's get a dataset
#We'll be using the "Arthritis" dataset

arth138<-Arthritis[,c(2,3,5)]
arth138.2<-data.frame(expand.grid(Sex=factor(c("F","M")),Trt=factor(c("Placebo","Treated")),Imp=factor(c("None","Some","Marked"))),count=as.numeric(table(arth138)))

#We will fit all 9 possible models and use AIC selection

our.models<-list()
our.models[[1]]<-glm(count~Sex+Trt+Imp,family=poisson,data=arth138.2)
our.models[[2]]<-glm(count~Sex+Trt+Imp+Sex:Trt,family=poisson,data=arth138.2)
our.models[[3]]<-glm(count~Sex+Trt+Imp+Sex:Imp,family=poisson,data=arth138.2)
our.models[[4]]<-glm(count~Sex+Trt+Imp+Trt:Imp,family=poisson,data=arth138.2)
our.models[[5]]<-glm(count~Sex+Trt+Imp+Sex:Trt+Sex:Imp,family=poisson,data=arth138.2)
our.models[[6]]<-glm(count~Sex+Trt+Imp+Sex:Trt+Trt:Imp,family=poisson,data=arth138.2)
our.models[[7]]<-glm(count~Sex+Trt+Imp+Sex:Imp+Trt:Imp,family=poisson,data=arth138.2)
our.models[[8]]<-glm(count~Sex+Trt+Imp+Sex:Trt+Sex:Imp+Trt:Imp,family=poisson,data=arth138.2)
our.models[[9]]<-glm(count~Sex+Trt+Imp+Sex:Trt+Sex:Imp+Trt:Imp+Sex:Trt:Imp,family=poisson,data=arth138.2)

#View models by AIC
plot(unlist(sapply(1:9,function(x)our.models[[x]]$aic)),xlab="Model #",ylab="AIC",type='b')

#View models by p-value
plot(unlist(sapply(1:9,function(x)1-pchisq(our.models[[x]]$deviance,our.models[[x]]$df.residual))),xlab="Model #",ylab="p-value",type='b',ylim=c(0,1))

#Therefore, select model 8
#Produce a model summary

summary(our.models[[8]])

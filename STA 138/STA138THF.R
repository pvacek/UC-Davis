#STA 138 Take Home Final

#Problem 4.11

coronary<-data.frame(expand.grid(Age=factor(c("35-44","45-54","55-64","65-74","75-84")),Smoke=factor(c("Nonsmokers","Smokers")),Death=factor(c("Person-Years","Coronary Deaths"))),
           count=c(18793,10673,5710,2585,1462,52407,43248,28612,12663,5317,
                   2,12,28,28,31,32,104,206,186,102))

#Part a.

#Create ratios here, IDK WTF person-years is is this a fucking joke

#Part b.

#Fit some sort of weird-ass GLM

#Part c.

#?????????????????????????????????????????????????????????

#Problem 5.15

#Problem 6.4

marijuana<-read.table("marijuana.txt",header=TRUE)

#Make into logistic dataframe

marijuana.dfs<-lapply(1:32,function(x){
  df.piece<-marijuana[rep(x,marijuana$count[x]),]-1
  return(df.piece[,1:4])
})

marijuana.log<-do.call(rbind,marijuana.dfs)
row.names(marijuana.log)<-NULL

#Use bic.glm for model selection

marijuana.bic<-bic.glm(m~.,data=marijuana.log,glm.family="binomial")
summary(marijuana.bic)

#Choose A-C model

#Create GLM
marijuana.model<-glm(m~a+c,data=marijuana.log,family="binomial")
summary(marijuana.model)

#Problem 6.8

sorethroat<-read.table("sorethroat.txt",header=TRUE)

#Use bic.glm for model selection

sorethroat.bic<-bic.glm(Y~.,data=sorethroat,glm.family="binomial")
summary(sorethroat.bic)

#Both models are similar, so we go with the intended model with both variables

sorethroat.glm<-glm(Y~.,data=sorethroat,family="binomial")
summary(sorethroat.glm)

#Interpret estimates and conduct inference about effects

#Problem 9.8

seatbelt4d<-data.frame(expand.grid(G=factor(c("Female","Male")),I=factor(c("No","Yes")),
                      L=factor(c("Urban","Rural")),S=factor(c("No","Yes"))),
                      count=c(7287,10381,996,812,3246,6123,973,1084,
                              11587,10969,759,380,6134,6693,757,513))
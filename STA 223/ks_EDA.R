library(data.table)
library(lubridate)

setwd("../Desktop")

ks<-fread("ks-projects-201801.csv")

ks$outcome<-ifelse(ks$state=="successful",1,ifelse(ks$state=="live"|ks$state=="undefined",-1,0))

ks<-ks[ks$outcome!=-1,]

ks$launched<-ymd(gsub(" .+","",ks$launched))

ks$deadline<-ymd(ks$deadline)

ks$duration<-as.numeric(ks$deadline-ks$launched)

ks$launchyr<-year(ks$launched)

ks$launchmo<-month(ks$launched)

ks<-ks[(ks$launchyr>1970)&(ks$launchyr<2018),]

hist(ks$duration,breaks=100)

##Success rate by month vs. normalized launch rate

ks_succrate<-aggregate(outcome~launchmo+launchyr,ks,mean)

ks_nproj<-aggregate(outcome~launchmo+launchyr,ks,length)

ks_succ_ts<-ts(ks_succrate[,3],start=c(2009,4),end=c(2017,12),frequency=12)

ts_dates<-rep(2009:2017,each=12)+rep(0:11,9)/12
ts_dates<-ts_dates[-c(1:3)]

plot(ks_succ_ts)
lines(smooth.spline(ts_dates,ks_succrate[,3], spar = 0.8), col = "blue",lwd=2)

plot(ks_nproj[,3],ks_succrate[,3],xlab="Projects Per Month",ylab="Success Rate")
lines(smooth.spline(ks_nproj[,3],ks_succrate[,3], spar = 1.1), col = "blue",lwd=2)

#Examining pledge average to see if spending characteristics differed across time

ks$avgpledge<-ks$pledged/pmax(ks$backers,rep(1,nrow(ks)))

#Examining categories vs. subcategories
cat_mat<-table(ks$main_category,ks$category)

#Which subcategories are represented in multiple categories?
cat_mat[,which(apply(cat_mat>0,2,sum)>1)]

#Send ks to csv
write.csv(ks,file="ks_v1.csv",row.names=FALSE)

###
#Examining August 2014
###
ks714<-ks[ks$launchyr==2014 & ks$launchmo==7,]

write.csv(ks714,file="ks714_v1.csv",row.names=FALSE)

#What was the success rate by country?
country_rate<-prop.table(table(ks714$country,ks714$outcome),margin=1)

#Plot below
barplot(country_rate[,2])

#How does the success rate look like with respect to duration?
dur_avgs<-aggregate(outcome~duration,ks714,mean)
plot(dur_avgs)
lines(smooth.spline(dur_avgs[,1],dur_avgs[,2], spar = 0.9), col = "blue",lwd=2)

#How does the success rate look for log goal?

plot(x=log(ks714$usd_goal_real),y=ks714_v2$outcome)
lines(x=sort(ks714_v2$log_goal),y=model_1pihat, col = "blue",lwd=2)

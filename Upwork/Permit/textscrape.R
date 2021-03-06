library(stringr)
library(pdftools)

#Scrape by text in the best way possible

text_scrape<-function(raw,state=""){
  lean<-gsub("[ ]{2,}"," ",gsub("[^0-9 A-z]","",raw))
  hmatches<-str_extract_all(lean,"[A-Z]{1,3}-{0,2}[0-9]{1,4} [NWES]{1,2}B*")[[1]]
  or<-as.character(guess_orientation(hmatches))
  n<-length(or)
  routes<-str_extract(hmatches,"^[A-Z0-9]+")
  route_df<-data.frame(miles=NA,route=c("",routes,""),to=NA,distance=NA,est_time=NA,orientation=c(NA,or,NA),
                       type=c("Stateline",rep("Highway",n),"Stateline"))
  route_df$route<-route_clean(route_df$route,state)
  return(route_df)
}
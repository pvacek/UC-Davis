
library(stringr)
library(pdftools)
library(tabulizer)

#Scraping the table if it is present

strip_numbers<-function(miles){
  as.numeric(gsub("[^0-9.]","",miles))
}

#When obtaining NA's, keep original value

revert_NA<-function(new,old){
  which_NA<-which(is.na(new))
  new[which_NA]<-old[which_NA]
  return(new)
}

state_route<-function(route,state=""){
  route_old<-as.character(route)
  route_new<-str_extract(route_old,"[A-Z]{1,3}-{0,1}[0-9]{1,5}")
  route_fix<-revert_NA(route_new,route_old)
  #Miscellaneous fixes to the route variable
  route_fix<-route_clean(route_fix,state)
  return(route_fix)
}

same_highway<-function(route_df){
  dupes<-which(diff(as.numeric(factor(route_df$route)))==0)
  return(route_df[-dupes,])
}

replace_val<-function(val,df,i,j){
  df[i,j]<-val
}

fix_bleeding<-function(pdfdf){
  badidx<-which(apply(pdfdf!="",1,sum)==1)
  badcol<-sapply(badidx,function(i)which(pdfdf[i,]!=""))
  for(i in 1:length(badidx))
    pdfdf[badidx[i]-1,badcol[i]]<-paste(pdfdf[badidx[i]-1,badcol[i]],pdfdf[badidx[i],badcol[i]])
  return(pdfdf[-badidx,])
}

ormap<-data.frame(Full=c("WEST","NORTH","EAST","SOUTH","SOUTHWEST","SOUTHEAST","NORTHWEST","NORTHEAST"),Short=c("W","N","E","S",
                                                                                                                "SW","SE","NW","NE"))

guess_orientation<-function(route){
  route<-gsub("B","",route)
  route_or<-toupper(str_extract(route,"[A-z]*?$"))
  shortmap<-match(route_or,ormap$Short)
  fullmap<-match(route_or,ormap$Full)
  if(sum(is.na(shortmap))<sum(is.na(fullmap))){
    ormap$Short[shortmap]
  }
  else{
    ormap$Short[fullmap]
  }
}

guess_stateline<-function(pdfto){
  pdf_tb<-pdfto[c(1,length(pdfto))]
  is_sl<-grepl(paste0(state_regex,collapse="|"),pdf_tb)
  sl<-ifelse(is_sl,"Stateline","Address")
  return(c(sl[1],rep("Highway",length(pdfto)-2),sl[2]))
}

route_clean<-function(route,state=""){
  route<-gsub("-","",route)
  route<-gsub("SL|LP","LOOP",route)
  route<-gsub("SH|SR",state,route)
  route<-gsub("IH","I",route)
  return(route)
}

extract_route<-function(tbl,state=""){
  route_df_raw<-do.call(rbind,tbl[sapply(tbl,miles_test)])
  #Detect any 'bleeding' rows due to column length restrictions
  route_df<-data.frame(fix_bleeding(route_df_raw))
  names(route_df)<-c("miles","route","to","distance","est_time")
  #Do all the data cleaning with regular expressions
  route_df$orientation<-guess_orientation(route_df$route)
  route_df$route<-state_route(route_df$route,state)
  route_df<-same_highway(route_df)
  route_df$distance<-strip_numbers(route_df$distance)
  route_df$distance[1]<-0
  route_df$miles<-c(0,diff(route_df$distance))
  route_df$miles<-ifelse(route_df$miles<0,0,route_df$miles)
  route_df$type<-guess_stateline(route_df$to)
  route_df<-route_df[,is.na(names(route_df))==FALSE]
  return(route_df)
}

table_handler<-function(pdf){
  route_tbl<-extract_tables(pdf)
  if(any(sapply(route_tbl,miles_test))){
    route_df<-tryCatch(extract_route(pdf),error=function(e)print("Table Extraction Failed"))
    return(route_df)
}
}
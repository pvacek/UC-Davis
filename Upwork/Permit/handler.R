#Permit Tool:

#Backend: Handler script

require(pdftools)
require(stringr)
require(RCurl)
require(tesseract)

gitsource<-function(u){
  script<-getURL(u,ssl.verifypeer = FALSE)
  eval(parse(text=script), envir= .GlobalEnv)
}

gitsource("https://raw.githubusercontent.com/pvacek/UC-Davis/master/Upwork/Permit/statecheck.R")
gitsource("https://raw.githubusercontent.com/pvacek/UC-Davis/master/Upwork/Permit/textscrape.R")
gitsource("https://raw.githubusercontent.com/pvacek/UC-Davis/master/Upwork/Permit/tablescrape.R")

null_data<-function(n=3){
  route_df<-data.frame(miles=NA,route=c("",rep("",n),""),to=NA,distance=NA,est_time=NA,orientation=c(NA,rep("",n=3),NA),
                       type=c("Stateline",rep("Highway",n=3),"Stateline"),stringsAsFactors = TRUE)
}

pdf_read<-function(pdf){
  return(paste0(pdf_text(pdf),collapse=" "))
}

ocr_read<-function(pdf){
  tryCatch(ocr(pdf),error=function(e)print(NULL))
}

miles_test<-function(tbl,k=0.3){
  ###
  #Test if the dataset has a miles column
  #If the proportion of numeric entries in the colum exceeds k=0.5, then it is evidence that it is a relevant table
  ###
  n<-nrow(tbl)
  n_miles<-sum(grepl("^[0-9. <>-]+$",tbl[,1]))
  ifelse((n_miles/n)>k,TRUE,FALSE)
}

table_step<-function(tbl,state=""){
  if(any(sapply(tbl,miles_test))){
    route_df<-tryCatch(extract_route(tbl,state),error=function(e)print(NULL))
    return(route_df)
  }
  else{
    return(NULL)
  }
}

handler<-function(row,r=100){
  #r is the number of characters at which we switch to OCR. Most permits seem to have at least 500 characters
  print(paste0("Reading ",row$name))
  #Step I: Extract pdf datapath
  pdf<-as.character(row$datapath)
  #Step II: Extract state
  state<-ifelse(!is.na(match(toupper(as.character(row$state)),state.abb)),as.character(row$state),state_check(pdf))
  print(paste0("Guessed State: ",state))
  #Step III: Try getting the table from the pdf
  print("Attempting route extraction via tables.")
  print(pdf)
  tbl<-extract_tables(pdf)
  if(length(tbl)>0){
    route_df<-table_step(tbl,state)
  }
  else{
    route_df<-NULL
  }
  #Step IV: Try extracting a pattern from the text
  if(is.null(route_df)){
    print("Trying route extraction via text...")
    raw<-pdf_read(pdf)
    if(nchar(raw)>=r){
      route_df<-text_scrape(raw,state)
    }
    else{
      #Step V: If text is not present, try tesseract-OCR
      print("PDF is an image. Trying Tesseract OCR...")
      raw_ocr<-ocr_read(pdf)
      if(!is.null(raw_ocr)){
        route_df<-text_scrape(raw_ocr,state)
      }
      else{
        print("OCR Failed. Returning Null Dataset")
        route_df<-null_data()
      }
    }
  }
  if(nrow(route_df)<=2 || is.null(route_df)){
    route_df<-null_data()
  }
  route_df<-data.frame(lapply(route_df,as.character),Order=1:nrow(route_df),Keep=TRUE,State=state,stringsAsFactors = FALSE)
  print(route_df)
  return(route_df)
}


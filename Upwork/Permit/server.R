#Permit Tool: Server

require(shiny)
require(rhandsontable)
require(pdftools)
require(stringr)
require(RCurl)

gitsource<-function(u){
  script<-getURL(u,ssl.verifypeer = FALSE)
  eval(parse(text=script), envir= .GlobalEnv)
}

gitsource("https://raw.githubusercontent.com/pvacek/UC-Davis/master/Upwork/Permit/statecheck.R")
gitsource("https://raw.githubusercontent.com/pvacek/UC-Davis/master/Upwork/Permit/textscrape.R")
gitsource("https://raw.githubusercontent.com/pvacek/UC-Davis/master/Upwork/Permit/tablescrape.R")

default=read.csv(text=getURL("https://raw.githubusercontent.com/pvacek/UC-Davis/master/Upwork/Permit/ptest.csv"))

pdf_read<-function(pdf){
  return(paste0(pdf_text(pdf),collapse=" "))
}

tablemaker<-function(pdf,state=""){
  route_tbl<-extract_tables(pdf)
  if(any(sapply(route_tbl,miles_test))){
    route_df<-tryCatch(extract_route(pdf,state),error=function(e)print("Table Extraction Failed"))
    return(route_df)
  }
  route_df<-text_scrape(pdf,state)
  return(route_df)
}

handler<-function(row){
  pdf<-as.character(row$datapath)
  state<-ifelse(!is.na(match(row$state,state.abb)),row$state,state_check(pdf))
  route_df<-tablemaker(pdf,state)
  route_df$Order<-1:nrow(route_df)
  route_df$Keep<-TRUE
  route_df$State<-state
  return(route_df)
}

server<-function(input,output,session){
  tokeep<-TRUE
  data <- reactive({
    if (is.null(input$files)) {
      DF = default
    } else {
      print(input$files)
      inFiles<-input$files
      n<-nrow(inFiles)
      DF = data.frame(Order=1:n,Keep=rep(TRUE,n),name=inFiles$name,datapath=inFiles$datapath,state=rep("",n))
    }
    if(input$filter){
      DFH<-hot_to_r(input$hot)
      DF<-DFH[DFH$Keep,]
    }
    if(input$order){
      DFO<-hot_to_r(input$hot)
      DF<-DFO[order(DFO$Order),]
    }
    DF
  })
  pdfdata<-reactive({
    result_df<-PDFtoCSV()
    if(input$filter2){
      keep_df<-hot_to_r(input$pdfdf)
      result_df<-keep_df[keep_df$Keep,]
    }
    if(input$order2){
      order_df<-hot_to_r(input$pdfdf)
      order_df<-order_df[order(order_df$Order),]
    }
    return(result_df)
  })
  output$hot <- renderRHandsontable({
    DF <- data()
    rhandsontable(DF,selectCallback = TRUE,readOnly=FALSE)
  })
  output$selected<-renderPrint({
    print(input$pdfdf)
  })
  output$pdfdf<-renderRHandsontable({
    DF<-pdfdata()
    rhandsontable(DF,selectCallback = TRUE,readOnly=FALSE)
  })
  PDFtoCSV<-eventReactive(input$execute,{
    DF<-hot_to_r(input$hot)
    result<-lapply(1:nrow(DF),function(i)handler(DF[i,]))
    result_df<-do.call(rbind,result)
    result_df
  })
  observeEvent(input$save,{
    DF<-pdfdata()
    DF$Order<-NULL
    DF$Keep<-NULL
    write.csv(DF,file=paste0(input$fname,".csv"),row.names=FALSE)
  })
}
#Permit Tool: UI
#This first version will start with one pdf

library(shiny)
library(rhandsontable)

fluidPage(
  titlePanel("Permit Tool"),
  navlistPanel("Tabs",
              tabPanel("Input",column(rHandsontableOutput("hot"),
                                      fileInput("files", "Choose Pdf File",accept = c(".pdf"),multiple=TRUE),
                                      actionButton("execute","Execute"),
                                      checkboxInput("filter", label = "Filter Rows", value = FALSE),
                                      checkboxInput("order", label = "Order Rows", value = FALSE),width=12)),
              tabPanel("Output",column(rHandsontableOutput("pdfdf"),
                                       checkboxInput("filter2", label = "Filter Rows", value = FALSE),
                                       checkboxInput("order2", label = "Order Rows", value = FALSE),
                                       textInput("fname","Save File As",value=""),
                                       actionButton("save","Save"),width=12)),
              tabPanel("Debug",column(verbatimTextOutput('selected'),width=12)),widths=c(2,10))

)
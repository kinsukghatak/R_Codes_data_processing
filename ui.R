##******** Upload a CSV file and then you can process !!  **********

library(shiny)
shinyUI(fluidPage(
  
  #Title Panel :
  #titlePanel(title=h4("Centering application for MMx",align="center"),windowTitle = "Centering App for MMx"),
  titlePanel(title=div(img(src="Kinsuk.gif"), "Centering Application for MMX")),
  sidebarLayout(
    
    #SidearPanel
    sidebarPanel(
      fileInput(inputId="file",label="Upload the CSV file"),# To upload the file ; gives the file up;oad button 
      helpText("Default max. file size is 9 MB"),
      #tags$hr(),
      helpText("Please keep the Geo/Region/Area column name as Region only"),
      #tags$hr(),
      helpText("Use _ instead of space in variable names"),
      #tags$hr(),
      h5(helpText("Select the read.table() parameters below")),
      checkboxInput(inputId="header",label="Header",value=FALSE),
      checkboxInput(inputId="stringsAsFactors",label="Strings As Factors", value=FALSE),
      br(),
      uiOutput("v1"),
      radioButtons(inputId="method",label="Methodology of transformation",choices=c(Mean_Cent="Mean Centering",Min_Cent="Min. Centering",Normalization="Normalization"),selected="Mean Centering"),
      br(),
      downloadButton(outputId= "downloadData_mean", label= "Download Centered Data set"),
      br()
      
     
      # selectInput("variable1",label="Select the x variable :",choices=names(data())) #,
      # # br(),
     
      
         ),
    
    
    
    ##Main Panel :
    mainPanel(
      
      
      uiOutput("tb")
  
          )
    
    
  )
)
)



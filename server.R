##******** Upload a CSV file and then you can visualise !!  **********

library(shiny)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(tidyr)
#library(lubridate)
#library(plotly)

options(shiny.maxRequestSize=9*1024^2)
shinyServer(
  
  function(input,output){
    
    ## The reactive() function will take file Data Frame as input from UI to use for further processing
  
    
    data<-reactive({
      
      #df1=data.frame()
      file1<-input$file   ## (file1 has all the information and meta data about file)
      if(is.null(file1)){return()}
      read.csv(file=file1$datapath,header=TRUE,stringsAsFactors =input$stringsAsFactors)
     
    })
    
   
    ## This reactive output contains the summary of meta data of the dataframe :
    
    output$filedf<-renderTable({
      if(is.null(data())){return()}
      input$file
    })
    
    
    ## This reactive output contains the summary of dataframe "data" / the actual content :
    
    output$summ<-renderTable({
      if(is.null(data())){return()}
      summary(data())
    })
    
    ##This reactive output displays the entire data in table format :
    
    output$table<-renderTable({
      if(is.null(data())){return()}
      data()
    })
    
    
    
    ## ** Let us now generate dynamic plots : 
    
    output$v1<-renderUI({
      selectInput("variable1","select the variables for Centering", choices = names(data()),multiple = TRUE)
    })
    
    
    
    ##This reactive output displays the entire data with mean cenetered format :
    
    ##output$MeanC<-renderDataTable
    
    data2<-reactive ({
      #if(is.null(data())){return()}
    
      #define the centering functions : 
      mean.center <- function(x){
        (x-mean(x[x>0],na.rm=TRUE)) 
      }
      
      min.center<-function(x){
        dplyr::if_else(x - min(x[x>0])<0,0,x - min(x[x>0]))
        
  
      }
      
      norm.center<-function(x){
        (x-mean(x[x>0],na.rm=TRUE))/sd(x,na.rm=TRUE)
      }
      
      df1=data.frame()
      
      
      #if(is.null(data())){return()}
      
      if (input$method == "Mean Centering")
      {
        df1<-data()%>% 
          group_by(Region)%>%
          ##generating new variables : 
          mutate_at(vars(input$variable1),
                    funs("MeanC"=mean.center)) %>%
          ungroup(Region) %>%
          as.data.frame()
        
      }
      
      else if (input$method == "Min. Centering")
      {
        df1<-data()%>% 
          group_by(Region)%>%
          ##generating new variables : 
          mutate_at(vars(input$variable1),
                    funs("MinC"=min.center)) %>%
          ungroup(Region) %>%
          as.data.frame()
        
      }
      
      else if (input$method == "Normalization")
      {
        df1<-data()%>% 
          group_by(Region)%>%
          ##generating new variables : 
          mutate_at(vars(input$variable1),
                    funs("Normal"=norm.center)) %>%
          ungroup(Region) %>%
          as.data.frame()
        
      }
      
      
      
        
      })
      
    output$MeanC<-renderDataTable({
      if(is.null(data2())){return()}
      return(data2())
    })
      
      
    
    ##Download : 
    
    output$downloadData_mean <- downloadHandler(
      
      filename=function(){paste("Centered data-", Sys.Date(), ".csv", sep="")} ,
      
      content = function(file){write.csv(data2(),file)}
      
    )
    
    
    
    

  
    ##This reactive output displays the entire data with minimum cenetered format :
    
    ## RenderUI() generates UI dynamically 
    # The following code genarates dynamic tabsets whenever the file is loaded
    output$tb<-renderUI({
      if(is.null(data()))
        h5("Developped by Kinsuk (AAC BLR)",tags$img(src='shinysnap.png',height=500,width=800))
      else
        tabsetPanel(tabPanel("About File",tableOutput("filedf")),tabPanel("Data",tableOutput("table")),tabPanel("Summary",tableOutput("summ")),tabPanel("Centered Variables",dataTableOutput("MeanC")))
      
    })
    
    
   
    
    
  })





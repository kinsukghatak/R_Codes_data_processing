#****Conversion from monthly data to daily & then to weekly data****

# $$$ Input : Start Date, End Date , Variables to be converted to weekly ones and
#variables which remains fixed over weeks (like SKUs, ND etc.)


##Setting the working directory (Where all your files are) :
setwd("C:\\Users\\ghki7001\\Desktop")

## Install these packages beforehand :

library(tidyverse)
library("xts")
library("lubridate")
library("dplyr")
library(readxl)
library(matrixStats)


##  Read the excel file with all the variables : **********
master1=read_excel("Model Brief_PMI.xlsx",sheet=1)
master1[is.na(master1)] <- 0   ## Replaces NAs with "0"

##Checking of different variable types using str() function:
str(master1)
master1$MonthYear<-as.character(format(as.Date(master1$Dates_Start), "%m-%Y"))

## Input 1 : Enter start and end date of your modelling period : 
startdate<-lubridate::date(dmy("06-01-2014"))
enddate<-lubridate::date(dmy("01-01-2017"))


## Taking all the variable names from the column names:
columns=colnames(master1)

##enter those variables here which you want to be converted from monthyl to weekly:
to_be_converted<-columns[-c(1,8:17,48)] ## Keeping aside Dates, NDs, SKUs
to_be_converted

##Enter those variables which you don't wanto to convert like the others and those which should remain fixed over weeks in a month:
not_to_be_converted<-columns[c(8:17)]  ## ** Just change the column nos.
not_to_be_converted

## Data with the not_to_be_converted variables : 
master2=master1[,c(8:17,48)] ## ** Similarly change the comlum nos.

##Conversion from monthly to daiyly for the converted variables: 
  dates<-seq(startdate,enddate,by='1 day')
  df<-dplyr::tbl_df(data.frame(Dates=dates))
  df<-df %>% 
    mutate(Months=month(Dates),
           Years=year(Dates),
           DateofMonth=day(Dates),
           MonthYear=format(as.Date(Dates), "%m-%Y")) %>% 
    group_by(Months,Years) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    inner_join(master1,by="MonthYear") %>% 
    mutate_at(.vars=vars(to_be_converted),.funs=funs(Daily=./count)) %>% 
    mutate(WD=weekdays(Dates)) %>%
    select(-MonthYear)
 
  
  dates<-seq(startdate,enddate,by='1 day')
  df2<-dplyr::tbl_df(data.frame(Dates=dates)) 
  df2<-df2 %>% 
    mutate(Months=month(Dates),
           Years=year(Dates),
           DateofMonth=day(Dates),
           MonthYear=format(as.Date(Dates), "%m-%Y")) %>% 
    group_by(Months,Years) %>% 
    mutate(count = n()) %>% 
    ungroup() %>% 
    inner_join(master2,by="MonthYear") %>% 
    mutate_at(.vars=vars(not_to_be_converted),.funs=funs(Daily=./1)) %>% 
    mutate(WD=weekdays(Dates)) %>%
    select(-MonthYear)
  
  names(df)
  grep("Daily",colnames(df))
  df[,grep("Daily",colnames(df))]
  
  names(df2)
  grep("Daily",colnames(df2))
  df2[,grep("Daily",colnames(df2))]
  
 

  ## ******** Conversion From Daily to weekly ****************** : 
  
  ## For the converted variables
  data1 <- as.xts(df[,grep("Daily",colnames(df))],order.by=as.Date(df$Dates,"%m/%d/%Y"))
  weekly1 <- apply.weekly(data1,colSums)
  names(weekly1)<-gsub("Daily","Weekly",names(weekly1))
  weekly1
  date(weekly1)
  weekly1_df=as.data.frame(weekly1)
  weekly1_df$Week_Ends=date(weekly1)
  str(weekly1_df)
  
  ## For the non converted variables (ND, SKU etc.)
  data2 <- as.xts(df2[,grep("Daily",colnames(df2))],order.by=as.Date(df2$Dates,"%m/%d/%Y"))
  weekly2 <- apply.weekly(data2,colMins)
  names(weekly2)<-gsub("Daily","Weekly",names(weekly2))
  weekly2
  weekly2_df=as.data.frame(weekly2)
  weekly2_df
  str(weekly2_df)
  weekly2_df$Week_Ends=date(weekly2)
  str(weekly2_df)
  
  
## ********** Merging all the variables in a single Data frame ************  
## Let's now merge the converted & non converted variables :
  
  Weekly_Final_df<- merge(weekly1_df, weekly2_df, by="Week_Ends")
  str(Weekly_Final_df)

## Writing the final results to Excel as " CSV" file : 
write.csv(Weekly_Final_df,"Weekly_PMI.csv")
  

##************** And you are done !! *****************

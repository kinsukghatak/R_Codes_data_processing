#****Conversion from monthly data to daily & then to weekly data****

# $$$ Input : Start Date, End Date , Variables to be converted to weekly ones and
#variables which remains fixed over weeks (like SKUs, ND etc.)


## We are not determining price directly. Instead we are calculating Value and volume & then going for price each time.



##Setting the working directory (Where all your files are) :
setwd("C:\\Users\\ghki7001\\Desktop")

## Install these packages beforehand :
install.packages('tidyverse')
library(tidyverse)
library("xts")
library("lubridate")
library("dplyr")
library(readxl)
library(matrixStats)


##  Read the excel file with all the variables : **********
master1=read_excel("Model Brief_PMI.xlsx",sheet=1)
master1<-read.csv("r1data_py.csv")
master1[is.na(master1)] <- 0   ## Replaces NAs with "0"

##Checking of different variable types using str() function:
str(master1)
master1$MonthYear<-as.character(format(as.Date(master1$DATE), "%m-%Y"))

## Input 1 : Enter start and end date of your modelling period : 
startdate<-lubridate::date(dmy("05-01-2013"))
enddate<-lubridate::date(dmy("30-04-2016"))


## Taking all the variable names from the column names:
columns=colnames(master1)

##enter those variables here which you want to be converted from monthyl to weekly:
to_be_converted<-columns[-c(1:3)] ## Keeping aside Dates, NDs, SKUs
to_be_converted

##Enter those variables which you don't wanto to convert like the others and those which should remain fixed over weeks in a month:
not_to_be_converted<-columns[c(1:3)]  ## ** Just change the column nos.
not_to_be_converted

## Data with the not_to_be_converted variables : 
master2=master1[,c(1:3)] ## ** Similarly change the comlum nos.

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
  
 
  ## ******** Conversion From Daily to weekly & then to monthly & yearly ****************** : 
  
  ## For the converted variables
  data1 <- as.xts(df[,grep("Daily",colnames(df))],order.by=as.Date(df$Dates,"%m/%d/%Y"))
  weekly1 <- apply.weekly(data1,colSums)
  
  ## Conversion to monthly & yearly :
  monthly1<-apply.monthly(weekly1,colSums)
  yearly1<-apply.yearly(weekly1,colSums)
  names(weekly1)<-gsub("M_Daily","Weekly",names(weekly1))
  names(monthly1)<-gsub("M_Daily","monthly",names(monthly1))
  names(yearly1)<-gsub("M_Daily","yearly",names(yearly1))
  weekly1
  date(weekly1)
  weekly1_df=as.data.frame(weekly1)
  weekly1_df$Week_Ends=date(weekly1)
  
  monthly1_df=as.data.frame(monthly1)
  monthly1_df$Week_Ends=date(monthly1)
  str(monthly1_df)
  
  yearly1_df=as.data.frame(yearly1)
  yearly1_df$Week_Ends=date(yearly1)
  str(yearly1_df)
  
  ## For the non converted variables (ND, SKU etc.)
  data2 <- as.xts(df2[,grep("Daily",colnames(df2))],order.by=as.Date(df2$Dates,"%m/%d/%Y"))
  weekly2 <-apply.weekly(data2,colMins)
  monthly2<-apply.monthly(weekly2,colMins)
  yearly2<-apply.yearly(weekly2,colMins)
  names(weekly2)<-gsub("M_Daily","Weekly",names(weekly2))
  names(monthly2)<-gsub("M_Daily","monthly",names(monthly2))
  names(yearly2)<-gsub("M_Daily","yearly",names(yearly2))
  weekly2
  weekly2_df=as.data.frame(weekly2)
  weekly2_df
  str(weekly2_df)
  weekly2_df$Week_Ends=date(weekly2)
  str(weekly2_df)
  
  monthly2_df=as.data.frame(monthly2)
  monthly2_df$Week_Ends=date(monthly2)
  str(monthly2_df)
  
  yearly2_df=as.data.frame(yearly2)
  yearly2_df$Week_Ends=date(yearly2)
  str(yearly2_df)
  
  
## ********** Merging all the variables in a single Data frame ************  
## Let's now merge the converted & non converted variables :
  
  weekly_final_df<- merge(weekly1_df, weekly2_df, by="Week_Ends")
  str(weekly_final_df)
  
  monthly_final_df<- merge(monthly1_df, monthly2_df, by="Week_Ends")
  str(monthly_final_df)

  yearly_final_df<- merge(yearly1_df, yearly2_df, by="Week_Ends")
  str(yearly_final_df)
  
  
  ## Rearranging the new Data frame with the column sequence of the master file:
  ## Fix the target as original sequence of the month data file
  
target_weekly<-gsub(".{1}$","Weekly",colnames(master1))  ## {replacing "_M" by "_Weekly"}
target_weekly<-target_weekly[-c(4:8,58)] ##{ removing the MonthYear Column}
target_weekly[1]<-colnames(weekly_final_df[1]) ##{Matching the Dates_Start column}
target_weekly
colnames(weekly_final_df)

## Taking the ids of the diffrent columns and processing accrodingly : 

idx <- sapply(target_weekly, function(x) {
  which(colnames(weekly_final_df) == x)
})
idx

weekly_final_df <- weekly_final_df[,idx]



target_monthly<-gsub(".{1}$","monthly",colnames(master1))  ## {replacing "_M" by "_Weekly"}
target_monthly<-target_monthly[-c(4:8,58)] ##{ removing the MonthYear Column}
target_monthly[1]<-colnames(monthly_final_df[1]) ##{Matching the Dates_Start column}
target_monthly

## Taking the ids of the diffrent columns and processing accrodingly : 

idx <- sapply(target_monthly, function(x) {
  which(colnames(monthly_final_df) == x)
})
idx

monthly_final_df <- monthly_final_df[,idx]



target_yearly<-gsub(".{1}$","yearly",colnames(master1))  ## {replacing "_M" by "_Weekly"}
target_yearly<-target_yearly[-c(4:8,58)] ##{ removing the MonthYear Column}
target_yearly[1]<-colnames(yearly_final_df[1]) ##{Matching the Dates_Start column}
target_yearly

## Taking the ids of the diffrent columns and processing accrodingly for yearly : 

idx <- sapply(target_yearly, function(x) {
  which(colnames(yearly_final_df) == x)
})
idx

yearly_final_df <- yearly_final_df[,idx]


## Writing the final results to Excel as " CSV" file : 
write.csv(weekly_final_df,"Weekly_PMI.csv")

write.csv(monthly_final_df,"Monthly_PMI.csv")

write.csv(yearly_final_df,"Yearly_PMI.csv")


##************** And voillaaa !!  you are done !! *****************






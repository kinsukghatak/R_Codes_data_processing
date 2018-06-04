## **********  Data Processing : Mean and minimum centering *************

#Enter the Path :   
path = "C:\\Users\\ghki7001\\Desktop\\R Trial"
setwd(path)

library(readxl)
filename <- file.choose()
df=read_excel(filename,sheet=1)
name <- basename(filename)
name1<-gsub(".xlsx","",name)  # Replacing ".xlsx" 



# Input the variable names you want to do centering
#var1<-as.character()
var <- c("")
variable<-function(var){
  var1<-as.character(readline("Enter Variables in vector format |eg. c(v1,v2..) : "))
  var <- eval(parse(text = var1))
  var
}

input_var=variable()


## Choose the file and create the Data Frame:



# Input the variable names you want to do centering
#my.name <- readline(prompt="Enter name: ")
#var1<-readline(prompt="Enter Variables : ")
#var1
#var=var1
#var=c('RP','TPR')

library(dplyr)

#define mean and minimum centering functions : 
mean_center <- function(x){
  (x-mean(x,na.rm=TRUE)) 
}

min_center<-function(x){
  (x-min(x,na.rm=TRUE))
}

## Mean centering :
df_mean<-df %>% 
  group_by(Region)%>%
##generating new variables : 
  mutate_at(vars(input_var),
            funs("MeanC" = mean_center)) %>%
    ungroup(Region)

## Mimimum centering :
df_min <- df %>% 
  group_by(Region)%>%
  ##generating new variables : 
  mutate_at(vars(input_var),
            funs("MinC"=min_center)) %>%
  ungroup(Region)

rownames(df_mean) <- NULL


## Now write the CSV files :
write.csv(df_mean, paste0(name1,"_meanCentered.csv"))
write.csv(df_min, paste0(name1,"._minCentered.csv"))



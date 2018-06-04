setwd("C:\\Users\\ghki7001\\Desktop\\New RP\\MMx_Muse")
df<-read.csv("Muse_Sales.csv")
library("tidyverse")
df_channel<-read.csv("Muse_Channel_Info.csv")
df_area<-read.csv("Muse_Area.csv")
df_area<-rename(df_area, Area.CODE=Code)
df_Tenpo<-read.csv("Muse_TENPO.csv")
df_SKU<-read.csv("Muse_ITEM.csv")


df_Muse<-merge(df,df_SKU,by.x="JAN.CODE",by.y="JAN.CODE")
df_Muse_new<- merge(df_Tenpo,df_Muse, by.x = "Store.CODE", by.y = "Store.CODE")
df_Muse_channel1<- merge(df_Muse_new,df_channel, by.x = "Channel.CODE", by.y = "Code_Channel")
library(doParallel)
library(dplyr)
df_Muse_area_channel <- full_join(df_Muse_channel1, df_area, by=c("Area.CODE"))
head(df_Muse_area_channel)
#df_Muse_area_channel<- merge(df_Muse_channel1, df_area, by.x = "Area.CODE", by.y = "Code")
df_Muse_final<-df_Muse_area_channel[,-c(1:4)] 

df_Muse_Pivot<-df_Muse_final %>%
  group_by(Week) %>%
  arrange(Week)

df2_Muse<- df_Muse_Pivot %>% group_by(Week, Area , Channel_Name, MAIN.MAKER,RB.TYPE,R.B.FORM) %>% 
  select(vol1,unit,Value) %>% 
  summarise_each(funs(sum)) %>% 
  ungroup()

write.csv(df2_Muse,"Muse_Final_Category_Area_Channel.csv")

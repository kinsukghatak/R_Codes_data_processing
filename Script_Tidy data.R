#install.packages("doParallel")
library(doParallel)
setwd("C:\\Users\\ghki7001\\Desktop\\New RP\\MMX_Scholls")
df<-read.csv("Scholls_Sales.csv")
library("tidyverse")
df_channel<-read.csv("Channel_Info_Scholl's.csv")
df_area<-read.csv("Area_Info_Scholl's.csv")
df_Tenpo<-read.csv("Scholls_TENPO.csv")
df_SKU<-read.csv("SKU_Mapping_Scholl's.csv")

df_Scholls<-merge(df,df_SKU,by.x="JAN.CODE",by.y="SKU.Code")
df_Scholls_new<- merge(df_Tenpo,df_Scholls, by.x = "Store.CODE", by.y = "Store.CODE")
df_Scholls_channel1<- merge(df_Scholls_new,df_channel, by.x = "Channel.CODE", by.y = "Channel_Code")
df_Scholls_area_channel<- merge(df_Scholls_channel1, df_area, by.x = "Area.CODE", by.y = "Area_Code")
df_Scholls_final<-df_Scholls_area_channel[,-c(1:4)] 

df_Scholls_Pivot<-df_Scholls_final %>%
  group_by(Week) %>%
  arrange(Week)



df2_Scholls<- df_Scholls_Pivot %>% group_by(Week,Area_Name,Channel_Name,Brands,TDP.Group) %>% 
  #filter(Brands=="RB Brand") %>%
  select(unit,Value) %>% 
  summarise_each(funs(sum)) %>% 
  ungroup()

write.csv(df2_Scholls,"Scholls_Final_Area_Channel.csv")




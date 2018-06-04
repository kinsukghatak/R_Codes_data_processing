
setwd("C:\\Users\\ghki7001\\Desktop\\R Codes")

df<-read.csv("Outlier.csv")

to_be_split<-c("TDP_File")

min_c<-function(x){
  
  max<-quantile(x,0.75)+0.75*IQR(x)
  min<-quantile(x,0.25)-0.75*IQR(x)
  x<-x[x<max & x>min]
  return(min(x[x>0]))
  
}

min_c(df$TDP_File)

df %>% group_by(Region) %>% summarise_at(vars(TDP_TNA),funs(min_c))


## Let's now create a nbew data frame with the split variables : 
df_new <- df %>%
  group_by(Region) %>%
  apply(TDP_TNA,min_c)
  #mutate_at(.vars=vars(to_be_split),.funs=funs("_Y_1"=min_c)) %>%
  
  ungroup(Region) %>%
  as.data.frame()

write.csv(df_new,"df_new.csv")

min_c<-function(x){
  
  max<-quantile(x,0.75)+0.75*IQR(x)
  min<-quantile(x,0.25)-0.25*IQR(x)
  x<-x[x<max & x>min]
  return(min(x))
  
  }



min_c(Vector)

Vector<- c(-50,-60,0.578,1,2,5,6,7,8,12,13,16,18,50,2.5,9,1.5,65,80,15,21,22)
Q1<- quantile(Vector,0.25)
Q3<-quantile(Vector,0.75)
max<-Q3+IQR(Vector)*1.5
min<-Q1-IQR(Vector)*1.5

Outlier_max<- Vector[Vector>max] 
Outlier_min<- Vector[Vector<min]
Outlier_max
Outlier_min
Outlier<-c(Outlier_max,Outlier_min)

Vector-Outlier




Vector1<- Vector[Vector<max & Vector>min]
min(Vector1)

v<-Vector[Vector<max & Vector>min]
v

quantile(Vector,0.75)-quantile(Vector,0.25)
IQR(Vector)
boxplot(Vector)
?quantile



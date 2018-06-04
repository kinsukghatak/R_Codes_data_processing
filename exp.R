
## Code for Exploratory data analysis and pre processing :

library(tidyverse)
library("PerformanceAnalytics")
library(d3heatmap)
library(tidyverse)
library(readxl)
library(ggplot2)
library(matrixStats)
library(dplyr)
setwd("C:\\Users\\ghki7001\\Desktop")
##  Sheet = 1 : **********

##  Read the excel file with all the variables : **********
df=read_excel("Input File.xlsx",sheet=1)
df[is.na(df)] <- 0   ## Replaces NAs with "0"


## Dependent  Variable : Volume
##Visualizations : 
plot(density(df$Volume))
hist(df$Volume)
plot(df$Volume,type="l")
##Checking of variance in a columnwise manner
colVars(data.matrix(df))

library(d3heatmap)
d3heatmap(df[,-c(1:3)], scale="column",hclustfun = hclust, colors="Blues")

## Corelation analysis of the entire raw data set
## Corelation matrix : 
d3heatmap(cor(df[,-c(1:3)],use="complete.obs"), scale="column",hclustfun = hclust, colors="Blues")
library("PerformanceAnalytics")
##Visualizing entire data set at one go: 
chart.Correlation(df[,-c(1:3)], histogram=TRUE, pch=10)

cor_matrix<-df[,-c(1:3)] %>%   ## Drop variables with zero variance
    cor(use="complete.obs")

install.packages("corrplot")
library("corrplot")
corrplot(cor_matrix, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

# "complete.obs" used to deal with missingvalues. Required if NA is present in the dataset

library(Hmisc)
corr_mat<-Hmisc::rcorr(as.matrix(df[,-c(1:3)]), type = "pearson")
corr_mat

##Corelation value wise arrangement : 

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


corr_mat_flat<-dplyr::tbl_df(flattenCorrMatrix(corr_mat$r, corr_mat$P))



corr_mat_flat<-corr_mat_flat %>% 
  arrange(row,column,cor) %>% 
  mutate(signv=if_else(cor>0,"plus","minus"),
         abscor=abs(cor))

corr_mat_flat<-corr_mat_flat %>% 
  group_by(row,column) %>% 
  arrange(row,desc(abscor),column)

class(corr_mat_flat)


## Factor analysis : ## ***********

##install.packages("FactoMineR")
library("FactoMineR")
library("devtools")
##install_github("kassambara/factoextra")
library("factoextra")





my_data<-df[,-c(1:3)]

res.pca <- PCA(my_data,ncp=6,graph = TRUE)

print(res.pca)

eigenvalues <- res.pca$eig
head(eigenvalues[, 1:3])


##Generation of ScreePlot : 
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

##Coordinates of variables on the principal components : 
res.pca$var$coord

##Contributions of the variables to the principal components:
factor_matrix<- res.pca$var$contrib
nrow(factor_matrix)
factor_matrix[1,0]
factor_matrix
dim(factor_matrix)
rownames(factor_matrix[1,])

mm_f=NULL

for (i in 1:nrow(factor_matrix)){
  
  for(j in 1:ncol(factor_matrix)){
    value<-which.max(factor_matrix[i,])
    eigenvalue<-max(factor_matrix[i,])
    ##index<-which(factor_matrix[i,] == max(factor_matrix[i,]), arr.ind = TRUE)
    mm<-cbind(rownames(factor_matrix)[i],value,eigenvalue)
  }
  mm_f=rbind(mm_f,mm)
  }

mm_f
dim(mm_f)

colnames(mm_f)=c("Variable","Factor","EigenValue")
rownames(mm_f)<-NULL
mm_f_df<-data.frame(mm_f)
mm_f_df
as.matrix(mm_f,dimnames=list(NULL,c("Variables","Dimension")))

mmf<-as.data.frame(mm_f)

class(mmf)
mmf

library(dplyr)
filter(mmf, cell_type == "Dim.1")
filter(expr, cell_type == "hesc" | cell_type == "bj fibroblast")

which.max(factor_matrix[2,])


as.data.frame(factor_matrix)

factor_matrix<-factor_matrix %>% 
  group_by(row,column) %>% 
  arrange(row,desc(Dim.1),column)

class(factor_matrix)



fviz_pca_var(res.pca,col.var="contrib")+
  scale_color_gradient2(low="black",mid="blue",high="red",midpoint=55)+theme_bw()


fviz_pca_var(res.pca, alpha.var="contrib")+
  theme_minimal()




######################################################33
model1<-lm(Sales~Measured_TV_Non_Promo+SEARCH,data=df)
model2<-lm(Sales~Measured_TV_Non_Promo+Competitor,data=df)
full_model<-lm(Sales~.,data=df[,-1:-3])
summary(model1)
summary(model2)
summary(full_model)

car::vif(full_model)

boxplot(df$Sales)$out

df_wo_out<-df %>% 
  filter(Sales<min(boxplot(df$Sales)$out))


full_model_woout<-lm(Sales~.,data=df_wo_out[,-1:-3])
summary(full_model_woout)



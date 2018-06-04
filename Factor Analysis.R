install.packages('tidyverse')
library(tidyverse)
library("xts")
library("lubridate")
library("dplyr")
library(readxl)
library(matrixStats)


df_sleep=read_excel("MQ.xlsx",sheet="Sleep")
library("FactoMineR")
library("factoextra")
res.pca1 <- PCA(df_sleep, graph = FALSE)
print(res.pca1)
eig.val1 <- get_eigenvalue(res.pca1)
eig.val1
var1 <- get_pca_var(res.pca1)
var1
fviz_pca_var(res.pca1, col.var = "black")
fviz_pca_var(res.pca1, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Coordinates
head(var1$coord)
# Cos2: quality on the factore map
head(var1$cos2)
# Contributions to the principal components
head(var1$contrib)

df_home=read_excel("MQ.xlsx",sheet="Home")
res.pca2 <- PCA(df_home, graph = FALSE)
print(res.pca2)
eig.val2 <- get_eigenvalue(res.pca2)
eig.val2
var2 <- get_pca_var(res.pca2)
var2
fviz_pca_var(res.pca2, col.var = "black")
fviz_pca_var(res.pca2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))



df_out=read_excel("MQ.xlsx",sheet="Out")
res.pca3 <- PCA(df_out, graph = FALSE)
print(res.pca3)
eig.val3 <- get_eigenvalue(res.pca3)
eig.val3
var3 <- get_pca_var(res.pca3)
var3
fviz_pca_var(res.pca3, col.var = "black")
fviz_pca_var(res.pca3, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

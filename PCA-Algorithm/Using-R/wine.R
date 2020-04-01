install.packages("readxl")
install.packages("ggplot2")
install.packages("foreign")
install.packages("formatR")
install.packages("NbClust")
install.packages("fpc")
install.packages("cluster")
install.packages("factoextra")
library(factoextra)
library(cluster)
library(fpc)
library(NbClust)

library(rmarkdown)
library(readxl)
library(ggplot2)
library(foreign)
library(reshape2) 
library(formatR)
library(UsingR)

getwd()
wine <- read_csv("D:/ExcelR/assingments/PCA/wine.csv")
View(wine)
str(wine)
install.packages("corrplot")
library(corrplot)
summary(wine)
plot(wine)
wine_corr <- cor(wine)
summary(wine_corr)
corrplot(wine_corr,method="number")
describe(wine)

wine_data <- wine[,-1]

summary(wine_data)
norm_wine <- scale(wine_data)
dist_wine <- dist(norm_wine,method = "euclidean")

dist_wine
str(dist_wine)

hclust_wine <- hclust(dist_wine, method = "complete")

plot(hclust_wine)
plot(hclust_wine, hang = -1)
rect.hclust(hclust_wine,plot(hclust_wine, hang = -1),k=5, border = "blue")

hclust_group <- cutree(hclust_wine,k=5)
wine_data_hclust <- cbind(wine_data, hclust_group)
aggregate(wine_data_hclust, by= list(wine_data_hclust$hclust_group), FUN = mean)

kmean_wine <- kmeans(norm_wine,5)

str(kmean_wine)
plot(kmean_wine$cluster)
library(animation)
kmean_ani <- kmeans.ani(norm_wine,5)

wine_data_kmean <- cbind(wine_data, kmean_wine$cluster)

library(cluster)

clara_wine <- clara(norm_wine,k=5, metric = "euclidean")

clusplot(clara_wine)
str(clara_wine)
wine_data_clara <- cbind(wine_data, clara_wine$clustering)
library(cluster)

wine_pam <- pam(norm_wine,k=5)

clusplot(wine_pam)
str(wine_pam)
wine_data_pam <- cbind(wine_data,wine_pam$clustering)
cor(wine)
pcawine <- princomp(wine, cor = TRUE, scores = TRUE)

summary(pcawine)
str(pcawine)
loadings(pcawine)
plot(pcawine)
pca_data <- pcawine$scores[,1:6]

pca_wine_data <- cbind(wine,pca_data)

colnames(pca_wine_data)
pca_lm <- lm(Type ~ `Comp.1`+`Comp.2`+`Comp.3`+`Comp.4`+`Comp.5`+`Comp.6`, data = pca_wine_data)

summary(pca_lm)

norm_pca_wine <- scale(pca_data)

dist_pca_wine <- dist(norm_pca_wine, method = "euclidean")
dist_pca_wine
hclut_pca_wine <- hclust(dist_pca_wine,method = "complete")
plot(hclut_pca_wine)
plot(hclut_pca_wine, hang = -1)

rect.hclust(hclut_pca_wine,plot(hclut_pca_wine,hang = -1),k=4,border = "red")

pca_group_wine <- cutree(hclut_pca_wine,k=4)

pca_wine_final <- cbind(wine_data,pca_group_wine)

pca_wine_final
aggregate(pca_wine_final, by=list(pca_wine_final$pca_group_wine), FUN = mean)

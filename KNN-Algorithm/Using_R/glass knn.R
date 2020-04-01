install.packages("class")
library(class)
glass <- read_csv("D:/ExcelR/assingments/KNN/glass.csv")

View(glass)
attach(glass)
str(glass)


table(glass$Type)
glass$Type <- factor(glass$Type)
str(glass)

View(glass)

round(prop.table(table(glass$Type))*100,1)


norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

glass_n<- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
summary(glass_n)

set.seed(123)
ind <- sample(2, nrow(glass_n), replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_n[ind==1,]
glass_test <-  glass_n[ind==2,]

set.seed(123)
ind1 <- sample(2, nrow(glass), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- glass[ind1==1,10]
glass_test_labels <-  glass[ind1==2,10]

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels$Type, k=3)
table(glass_test_pred,glass_test_labels$Type)

mean(glass_test_pred==glass_test_labels$Type)

install.packages("gmodels")
library(gmodels)

CrossTable(x=glass_test_labels$Type,y=glass_test_pred,prop.chisq = FALSE) 


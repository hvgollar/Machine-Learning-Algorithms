install.packages("kernlab")
install.packages("caret")
library(kernlab)
library(caret)
install.packages("psych")
install.packages("e1071")
library(e1071)
library(plyr)
library(ggplot2)
library(psych)


forest <- read.csv("D:/ExcelR/assingments/Support Vector machine/forestfires.csv")
View(forest)

normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  # subtract the min value in x and divide by the range of values in x.
}

forest$temp <- normalise(forest$temp)
forest$rain <- normalise(forest$rain)
forest$RH <- normalise(forest$RH)
forest$wind <- normalise(forest$wind)

sum(forest$area < 5)
sum(mydata$area >= 5)

forest$size <- NULL
forest$size <- factor(ifelse(forest$area < 5, 1, 0),labels = c("small", "large"))
train <- forest[1:400,]
test <- forest[401:517,]
attach(forest)
model1<-ksvm(size ~ temp + RH + wind + rain ,data= train, kernel = "vanilladot")
pred1 <- predict(model1, test)
mean(pred1==test$size)


model2<-ksvm(size ~ temp + RH + wind + rain ,data= train, kernel = "rbfdot")
pred2<- predict(model2, test)
mean(pred2==test$size)

model3<-ksvm(size ~ temp + RH + wind + rain ,data= train, kernel = "polydot")
pred3<- predict(model3, test)
mean(pred3==test$size)

model4<-ksvm(size ~ temp + RH + wind + rain ,data= train, kernel = "tanhdot")
pred4<- predict(model4, test)
mean(pred4==test$size)


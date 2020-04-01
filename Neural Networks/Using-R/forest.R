forest <- read.csv("D:/ExcelR/assingments/Neural Networks/forest1fires.csv")
View(forest)
forest<- forest[,-c(1:6,12:31)]
forest$area <- factor(ifelse(forest$area < 6, 0, 1))
# temperature, rain, relative humidity and wind speed
summary(forest)
attach(forest)
str(forest)
View(forest)

install.packages("plyr")
library(plyr)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forest$temp <- normalize(forest$temp)
forest$rain <- normalize(forest$rain)
forest$RH <- normalize(forest$RH)
forest$wind <- normalize(forest$wind)


View(forest)

summary(forest$area)

forest_train<-forest[1:400,]

forest_test<-forest[401:517,]

install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  
library(nnet)

forest_model<- neuralnet(area~temp+wind+RH+rain, data=forest_train)
str(forest_model)

plot(forest_model)
summary(forest_model)
model_results <- compute(forest_model,forest_test[1:4])
str(model_results)
predicted_area<- model_results$net.result
cor(predicted_area,forest_test$area)

forest_model2 <- neuralnet(area~.,data =forest_train,hidden = 5)
plot(forest_model2)

str(forest_model2)
summary(forest_model2)
model_results1 <- compute(forest_model2,forest_test[1:4])
str(model_results1)
predicted_area1<- model_results1$net.result
cor(predicted_area1,forest_test$area)

forest1_model3 <- neuralnet(area~.,data =forest_train,hidden =8)
plot(forest1_model3)

str(forest1_model3)
summary(forest1_model3)
model_results2 <- compute(forest1_model3,forest_test[1:4])
str(model_results2)
predicted_area2<- model_results2$net.result
cor(predicted_area2,forest_test$area)
plot(predicted_area2,forest_test$area)

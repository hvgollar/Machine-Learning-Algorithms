Startups <- read_csv("D:/ExcelR/assingments/Neural Networks/50_Startups.csv")
View(Startups)
str(Startups)
attach(Startups)

summary(Startups)
plot(Startups)
install.packages("plyr")
library(plyr)

Startups$State <- as.numeric(revalue(Startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))

View(Startups)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups,FUN=normalize))


View(Startups_norm)


summary(Startups_norm$Profit)
summary(Startups$Profit)

startups_train<-Startups_norm[1:30,]
View(startups_train)
startups_test<-Startups_norm[31:50,]

install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  
library(nnet)

startups_model<- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State, data=startups_train)
str(startups_model)

plot(startups_model)
summary(startups_model)
model_results <- compute(startups_model,startups_test[1:4])
str(model_results)
predicted_profit<- model_results$net.result
cor(predicted_profit,startups_test$Profit)

Startups_model2 <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data =startups_train,hidden = 2)
plot(Startups_model2)

str(Startups_model2)
summary(Startups_model2)
model_results1 <- compute(Startups_model2,startups_test[1:4])
str(model_results1)
predicted_profit1<- model_results1$net.result
cor(predicted_profit1,startups_test$Profit)

Startups_model3 <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data =startups_train,hidden =3)
plot(Startups_model3)

str(Startups_model3)
summary(Startups_model3)
model_results2 <- compute(Startups_model3,startups_test[1:4])
str(model_results2)
predicted_profit2<- model_results2$net.result
cor(predicted_profit2,startups_test$Profit)
plot(predicted_profit2,startups_test$Profit)

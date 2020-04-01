install.packages("C50") 
install.packages("tree")

library(C50)
library(tree)

attach(Fraud_check)
str(Fraud_check)
Fraud_rand <- Fraud_check[order(runif(600)),]
Fraud_rand




Risky_Good = ifelse(Fraud_rand$Taxable.Income<= 30000, "Risky", "Good")

 
FC = data.frame(Fraud_rand,Risky_Good)
View(FC)

FC_train <- FC[1:400,]
FC_test <- FC[401:600,]

prop.table(table(FC_train$Risky_Good))
prop.table(table(FC_test$Risky_Good))

fraud_model <- C5.0(FC_train[,-c(7)],FC_train$Risky_Good)
summary(fraud_model)
plot(fraud_model)

fraud_pred <- predict(fraud_model,FC_test)

mean(fraud_pred==FC_test$Risky_Good) 


CrossTable(FC_test$Risky_Good,fraud_pred)



fraud_pred1 <- predict(fraud_model,FC_train)

mean(fraud_pred1==FC_train$Risky_Good) 


CrossTable(FC_train$Risky_Good,fraud_pred1)



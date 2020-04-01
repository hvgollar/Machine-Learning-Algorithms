install.packages("randomForest")
library(randomForest)
Company <- read.csv("D:/ExcelR/assingments/decesion tree/Company_Data.csv")
View(Company)
Sales_Result <- ifelse(Company$Sales > 7.490,"yes","no")

attach(Company)
str(Company)
CD= data.frame(Company,Sales_Result)
company_rand <- CD[order(runif(400)),]
company_rand
round(prop.table(table(company_rand$Sales_Result))*100,1)
View(company_rand)

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

company_n <- as.data.frame(lapply(company_rand[,-c(7,10,11,12)], norm))
View(company_n)
company_data<-data.frame(company_rand,company_n) 
View(company_data)
final_company <- company_data[,-c(1,2,3,4,5,6,8,9)]
View(final_company)

final_company_train <- final_company[1:250,]
final_company_test <- final_company[251:400,]


company_forest <- randomForest(Sales_Result~.,data = final_company,importance=TRUE,ntree=100)
company_forest

acc_company <- mean(final_company_test$Sales_Result==predict(company_forest,data=final_company_test))
acc_company

acc_companyT <- mean(final_company_train$Sales_Result==predict(company_forest,data=final_company_train))
acc_companyT

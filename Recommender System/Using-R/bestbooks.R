

install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
library("recommenderlab")
library(caTools)
library(ggplot2)

books<- read.csv(file.choose())
View(books)
books$rating=books$ratings...3. 
books$userid <- books$X.1
books<- books[,-c(1,2,6)]
str(books)

hist(books$rating)

books.copy <- books

books.copy$Book.Author <- as.numeric(books.copy$Book.Author)
books.copy$Publisher   <- as.numeric(books.copy$Publisher  )

books.copy = na.omit(books.copy)

#create sparse matrix
k = with(books.copy,sparseMatrix(i=Book.Author, 
                           j=Publisher, 
                           x=rating,
                           dimnames=list(levels(Book.Author), levels(Publisher))))


sparse_ratings <- sparseMatrix(i = books.copy$Book.Author,
                               j = books.copy$Publisher ,
                               x = books.copy$rating, 
                               dims = c(length(unique(books.copy$Book.Author)),
                                        length(unique(books.copy$Publisher))),  
                               dimnames = list(paste("u", 1:length(unique(books.copy$Book.Author)), sep = ""), 
                                               paste("m", 1:length(unique(books.copy$Publisher)), sep = "")))

rm(books.copy)

sparse_ratings[1:10,1:10]
#User Based Collaborative Filtering

ratingMat <- new("realRatingMatrix", data = sparse_ratings)
ratingMat

book_model1 <- Recommender(ratingMat, method="POPULAR")


recommended_items1 <- predict(book_model1, ratingMat[413:414], n=5)
as(recommended_items1, "list")

book_model2 <- Recommender(ratingMat, method="UBCF")


recommended_items2 <- predict(book_model2, ratingMat[413:414], n=5)
as(recommended_items2, "list")


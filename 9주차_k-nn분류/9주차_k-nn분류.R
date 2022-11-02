#packages("class")에서 k-nn()함수 불러오기
install.packages('class')
library(class)

data(iris3)
str(iris3) #3차원 데이터

#train데이터 만들기
train<-rbind(iris3[1:25,,1],iris3[1:25,,2],iris3[1:25,,3] )
train
#test데이터 만들기
test<-rbind(iris3[36:50,,1],iris3[26:50,,2],iris3[26:50,,3] )
test
#factor(c(rep())) 만들기
cl<-factor(c(rep("setosa", 25), rep("versicolor", 25), rep("virginica", 25)))

#knn모델 만들기
iris3_model<-knn(train, test, cl, k = 3, prob = TRUE)
iris3_model
#정오분류표 만들기
m<-table(cl, iris3_model, dnn=c("Actual", "Predicted"))
#오분률
1-sum(diag(m))/sum(m)


#knn(k=7)모델 만들기
iris3_model7<-knn(train, test, cl, k = 7, prob = TRUE)
iris3_model7
#(k=7)정오분류표 만들기
m7<-table(cl, iris3_model7, dnn=c("Actual", "Predicted"))
#(k=7)오분률
1-sum(diag(m7))/sum(m7)

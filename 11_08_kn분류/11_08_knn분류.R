data(iris)
idx<-sample(1:nrow(iris), as.integer(0.7*nrow(iris)))

train<-iris[idx,]
dim(train)
test<-iris[-idx,]
dim(test)

install.packages('class')
library(class)
iris_model<-knn(train[-5], test[-5], train$Species, k = 3)
table(iris_model)
table(iris_model, test$Species, dnn = ())

install.packages('kknn')
library(kknn)
m<-dim(iris)[1]
val<-sample(1:m, size = round(m/3), replace = F, prob = rep(1/m, m))
train<-iris[-val,]
dim(train)
test<-iris[val,]
dim(test)
cv<-train.kknn(Species~., train, test, ks = seq(1, 50, by = 2), scale = F)
cv

model<-kknn(Species~., train=train, test= test, distance = 1, kernel = "rectangular")
f1<-fitted(model)
t<-table(test$Species, f1)

sum(diag(t))/sum(t)

model<-kknn(Species~., train=train, test= test, distance = 1, kernel = "triangular")
f1<-fitted(model)
t<-table(test$Species, f1)
sum(diag(t))/sum(t)

model<-kknn(Species~., train=train, test= test, distance = 2, kernel = "rectangular")
f1<-fitted(model)
t<-table(test$Species, f1)
sum(diag(t))/sum(t)

model<-kknn(Species~., train=train, test= test, distance = 2, kernel = "triangular")
f1<-fitted(model)
t<-table(test$Species, f1)
sum(diag(t))/sum(t)

model<-kknn(Species~., train=train, test= test, distance = 3, kernel = "rectangular")
f1<-fitted(model)
t<-table(test$Species, f1)
sum(diag(t))/sum(t)

model<-kknn(Species~., train=train, test= test, distance = 3, kernel = "triangular")
f1<-fitted(model)
t<-table(test$Species, f1)
sum(diag(t))/sum(t)

?kknn
?pch

pcol<-as.character(as.numeric(test$Species))
pairs(test[1:4], pch = pcol, col=c('green3', 'red')[(test$Species != f1)+1])

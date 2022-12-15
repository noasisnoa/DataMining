data(iris)
install.packages("nnet")
library(nnet)
nn<-nnet(Species~.,data=iris, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
summary(nn)
source("https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/8
       5e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.")
plot.nnet(nn)
p<-predict(nn, iris, type = 'class')
table(iris$Species, p, dnn=c('Actual', 'Predicted'))
t<-table(iris$Species, p, dnn=c('Actual', 'Predicted'))
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
size2<-sum(diag(t))/sum(t)
size2

#Size 5
data(iris)
nn<-nnet(Species~.,data=iris, size = 5, rang = 0.1, decay = 5e-4, maxit = 200)
summary(nn)
plot.nnet(nn)
p<-predict(nn, iris, type = 'class')
table(iris$Species, p, dnn=c('Actual', 'Predicted'))
t<-table(iris$Species, p, dnn=c('Actual', 'Predicted'))
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
size5<-sum(diag(t))/sum(t)
size5

#Size 10
nn<-nnet(Species~.,data=iris, size = 10, rang = 0.1, decay = 5e-4, maxit = 200)
summary(nn)
plot.nnet(nn)
p<-predict(nn, iris, type = 'class')
table(iris$Species, p, dnn=c('Actual', 'Predicted'))
t<-table(iris$Species, p, dnn=c('Actual', 'Predicted'))
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
size10<-sum(diag(t))/sum(t)
size10

size<-c(size2, size5, size10)
label<-c(2, 5, 10)
data<-data.frame(label, size)
data
plot(data, xlab = 'size', ylab='Accuracy',type='l', main='size')



#Maxit 50
nn<-nnet(Species~.,data=iris, size = 2, rang = 0.1, decay = 5e-4, maxit = 50)
summary(nn)
plot.nnet(nn)
p<-predict(nn, iris, type = 'class')
t<-table(iris$Species, p, dnn=c('Actual', 'Predicted'))
t
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
Maxit50<-sum(diag(t))/sum(t)
Maxit50


#Maxit 200
nn<-nnet(Species~.,data=iris, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
summary(nn)
plot.nnet(nn)
p<-predict(nn, iris, type = 'class')
t<-table(iris$Species, p, dnn=c('Actual', 'Predicted'))
t
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
Maxit200<-sum(diag(t))/sum(t)
Maxit200

#Maxit 300
nn<-nnet(Species~.,data=iris, size = 2, rang = 0.1, decay = 5e-4, maxit = 300)
summary(nn)
plot.nnet(nn)
p<-predict(nn, iris, type = 'class')
t<-table(iris$Species, p, dnn=c('Actual', 'Predicted'))
t
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
Maxit300<-sum(diag(t))/sum(t)
Maxit300

maxit<-c(1, Maxit200, Maxit300)
label<-c(50, 200, 300)
maxit_data <- data.frame(label,maxit)
maxit_data
plot(maxit_data, ylim=c(0.94, 1),  xlab = 'maxit', ylab='Accuracy',type='l', main='maxit')




#train,test 80; 20으로 바꾸고 확인하기
set.seed(1234)
ind<-sample(2, nrow(iris), prob=c(0.8, 0.2), replace = TRUE)
train<-iris[ind==1,]
test<-iris[ind==2,]
dim(train)
dim(test)
str(test)

#train,test 100개; 50개로 바꾸고 확인하기
data(iris)
m<-dim(iris)[1]
m
val<-sample(1:m, size = round(m/3), replace = F)
train<-iris[-val,]
test<-iris[val,]
dim(train)
dim(test)


#train 인공신경망 분류
train_nn<-nnet(Species~., data=train, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
summary(train_nn)
plot.nnet(train_nn)
p<-predict(train_nn, newdata = test, type = 'class' )
p
table(p, test$Species, dnn = c("Actual", "Predicted"))
t<-table(p, test$Species, dnn = c("Actual", "Predicted"))
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
test_size2<-sum(diag(t))/sum(t)
test_size2

#train SIZE = 5
train_nn<-nnet(Species~., data=train, size = 5, rang = 0.1, decay = 5e-4, maxit = 200)
summary(train_nn)
plot.nnet(train_nn)
p<-predict(train_nn, newdata = test, type = 'class' )
p
table(p, test$Species, dnn = c("Actual", "Predicted"))
t<-table(p, test$Species, dnn = c("Actual", "Predicted"))
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
test_size5<-sum(diag(t))/sum(t)
test_size5

#train SIZE = 10
train_nn<-nnet(Species~., data=train, size = 10, rang = 0.1, decay = 5e-4, maxit = 200)
summary(train_nn)
plot.nnet(train_nn)
p<-predict(train_nn, newdata = test, type = 'class' )
p
table(p, test$Species, dnn = c("Actual", "Predicted"))
t<-table(p, test$Species, dnn = c("Actual", "Predicted"))
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
test_size10<-sum(diag(t))/sum(t)
test_size10


iris_size<-c(0.84, test_size5, test_size10)
label<-c(2, 5, 10)
iris_test_size<-data.frame(label, iris_size)
plot(iris_test_size, main='iris size',type='l', xlab='size', ylab='Accuracy')

#train MAXIT = 50
train_nn<-nnet(Species~., data=train, size = 2, rang = 0.1, decay = 5e-4, maxit = 50)
summary(train_nn)
plot.nnet(train_nn)
p<-predict(train_nn, newdata = test, type = 'class' )
p
table(p, test$Species, dnn = c("Actual", "Predicted"))
t<-table(p, test$Species, dnn = c("Actual", "Predicted"))
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
test_Maxit50<-sum(diag(t))/sum(t)
test_Maxit50

#train MAXIT = 200
train_nn<-nnet(Species~., data=train, size = 2, rang = 0.1, decay = 5e-4, maxit = 200)
summary(train_nn)
plot.nnet(train_nn)
p<-predict(train_nn, newdata = test, type = 'class' )
p
table(p, test$Species, dnn = c("Actual", "Predicted"))
t<-table(p, test$Species, dnn = c("Actual", "Predicted"))
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
test_Maxit200<-sum(diag(t))/sum(t)
test_Maxit200


#train MAXIT = 300
train_nn<-nnet(Species~., data=train, size = 2, rang = 0.1, decay = 5e-4, maxit = 300)
summary(train_nn)
plot.nnet(train_nn)
p<-predict(train_nn, newdata = test, type = 'class' )
p
table(p, test$Species, dnn = c("Actual", "Predicted"))
t<-table(p, test$Species, dnn = c("Actual", "Predicted"))
sum(diag(t))/sum(t)
1-(sum(diag(t))/sum(t))
test_Maxit300<-sum(diag(t))/sum(t)
test_Maxit300


iris_maxit<-c(0.98, 0.98, 0.96)
label<-c(50, 100, 200)
iris_test_maxit<-data.frame(label, iris_maxit)
plot(iris_test_maxit, main='iris maxit',type='l', xlab='size', ylab='Accuracy')



#nnet
sizeplot<-c(size5, size10)
plot(sizeplot)
plot(sizeplot, type = 'l')


Maxitplot<-c(Maxit50, Maxit200, Maxit300)
plot(Maxitplot)

#train test 
test_sizeplot<-c(test_size5, test_size10)
plot(sizeplot)
plot(sizeplot, type = 'l')


test_Maxitplot<-c(test_Maxit50, test_Maxit200, test_Maxit300)
plot(test_Maxitplot)




#install.packages("caret")
#library(caret)
#caret<-confusionMatrix(p, reference = test$Species, positive = '1')
#caret


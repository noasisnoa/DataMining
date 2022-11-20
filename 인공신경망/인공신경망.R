data(iris)
library(e1071)
##cost=10, gamma=0.1
f1_0.1<-svm(Species~., data=iris, type='C-classification', kernel='radial', cost='10', gamma=0.1)
f1_0.1
summary(f1_0.1)
p_0.1<-predict(f1_0.1, newdata=iris, decision.values=TRUE )
t_0.1<-table(p_0.1, iris$Species, dnn =c("Actual", "Predicted"))

#자세하게 세세하게 보인다.
classAgreement(t_0.1)
plot(f1_0.1, iris, Petal.Width~Petal.Length, slice=list(Sepal.Width=3, Sepal.Length=4))
